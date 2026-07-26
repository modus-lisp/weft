;;;; forms-elem-wave.lisp — the focused per-element WPT swarm.
;;;;
;;;; Was bash (forms-elem-wave.sh + forms-elem-worker.sh).  It outgrew bash the
;;;; moment workers stopped owning exactly one file: reading a worker's work is
;;;; now a git operation, keeping its best is a patch, and merging is a gated
;;;; apply — three things bash expresses as string mangling and Lisp expresses
;;;; as data.
;;;;
;;;; Each worker gets a real git WORKTREE of the source tree and may edit
;;;; ANYTHING in it.  That is the point: wave 4 went flat because the units had
;;;; hit their edit-scope ceiling — every residual failure lived in bridge.lisp
;;;; or dom.lisp, outside the one file a worker was allowed to touch — and the
;;;; single arm that gained did so by breaking the rule.  With a worktree we no
;;;; longer have to care which files a worker needs; `git diff` tells us what it
;;;; did, and TOTAL + the sentinels + scripting-dom decide whether we keep it.
;;;;
;;;; Usage:
;;;;   sbcl --non-interactive --load tools/swarm/forms-elem-wave.lisp \
;;;;        --eval '(weft.swarm:wave "/tmp/felem5")'

(require :asdf)                         ; for uiop; sb-thread is already built in

(defpackage :weft.swarm
  (:use :cl)
  (:export #:wave #:*worker-budget* #:*round-max* #:*units* #:*variants*))

(in-package :weft.swarm)

;;; ---- configuration --------------------------------------------------------

(defparameter *src*    "/home/claude/weft")
(defparameter *operandi-root*
  (or (uiop:getenv "OPERANDI_ROOT")
      (error "OPERANDI_ROOT must be set to the operandi checkout")))
(defparameter *wpt*    "/home/claude/wpt")

;;; Wave 6 onward the arms are spread across UNITS rather than across variants of
;;; the same unit.  Waves 1-5 ran 2 units x 3 variants because there was nothing
;;; else to work on; the corpus sweep (inspect/forms-oracle.lisp) opened four
;;; units with 150+ untouched subtests between them, and three arms racing on one
;;; unit only ever merges one patch.
(defparameter *variants* '(("a" . :flash))
  "Variants run per unit when a unit is named as a bare string in *UNITS*.")

(defparameter *units* '("forminfra" "label" "option" "select" "button")
  "Units to work this wave.  An entry may also be (unit variant . tier) for an
extra arm on a unit that deserves one — e.g. (\"option\" \"p\" . :pro).")

(defparameter *models* '((:flash . "deepseek/deepseek-v4-flash")
                         (:pro   . "deepseek/deepseek-v4-pro")))

(defparameter *worker-budget* 5400 "Total wall-clock seconds per worker.")
(defparameter *round-max* 1800     "Seconds any single agent invocation may run.")
(defparameter *dry-rounds* 2       "Rounds without a TOTAL gain before a worker stops.")

;;; operandi's defaults are sized for a small local model; both deepseek-v4
;;; models carry a 1,048,576-token window.  At the 24k default these workers
;;; THRASHED — 23 compactions and zero Write calls in 40 minutes.
(defparameter *context-budget* 200000)
(defparameter *max-iters* 150)

;;; Files the workers must not be able to edit their way to a better score
;;; through.  Restored from HEAD before every scoring run, so tampering is not
;;; punished, it is simply impossible.  (Assume any printed score gets
;;; optimised, including through its bugs — that lesson cost us a whole wave.)
(defparameter *harness-paths*
  '("inspect/forms-oracle.lisp" "tools/swarm"))

;;; ---- shell ----------------------------------------------------------------

(defvar *print-lock* (sb-thread:make-mutex :name "swarm-print"))

(defun note (fmt &rest args)
  (sb-thread:with-mutex (*print-lock*)
    (format t "~&~a~%" (apply #'format nil fmt args))
    (finish-output)))

(defun sh (fmt &rest args)
  "Run a shell command line; return (values trimmed-stdout exit-code stderr).
Stderr is captured rather than discarded: when a git step fails, its message is
the only thing that says why, and a gate that reports \"it failed\" without it
sends you looking in the wrong place."
  (multiple-value-bind (out err code)
      (uiop:run-program (apply #'format nil fmt args)
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :ignore-error-status t)
    (values out code err)))

(defun sh-ok (fmt &rest args)
  (zerop (nth-value 1 (apply #'sh fmt args))))

(defun slurp (path)
  (when (probe-file path)
    (with-open-file (s path :external-format :utf-8)
      (let ((str (make-string (file-length s))))
        (subseq str 0 (read-sequence str s))))))

(defun spit (path text)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create :external-format :utf-8)
    (write-string text s)))

;;; ---- parsing the oracle ---------------------------------------------------

(defun lines-starting (text prefix)
  (when text
    (with-input-from-string (s text)
      (loop for line = (read-line s nil)
            while line
            when (and (>= (length line) (length prefix))
                      (string= prefix line :end2 (length prefix)))
              collect line))))

(defun total-of (text)
  "The TOTAL from the last `TOTAL <n> passed ...' line, or NIL."
  (let ((line (car (last (lines-starting text "TOTAL ")))))
    (when line (parse-integer line :start 6 :junk-allowed t))))

(defun verdict-of (text)
  "The UNIT + TOTAL lines, joined — the one-line summary of a scoring run."
  (format nil "~{~a~^ | ~}"
          (append (lines-starting text "UNIT ") (lines-starting text "TOTAL "))))

;;; ---- jobs -----------------------------------------------------------------

(defstruct job id unit variant model wave)

(defun jp (job kind)
  "Path of one of JOB's artefacts.  The worktree is WAVE/ID; everything the
   oracle or the harness writes lives OUTSIDE it, as WAVE/ID.<kind>, so it can
   never turn up inside the worker's own diff."
  (let ((base (format nil "~a/~a" (job-wave job) (job-id job))))
    (if (eq kind :wd) base (format nil "~a.~(~a~)" base kind))))

(defun make-jobs (wave)
  (flet ((job (unit v tier)
           (make-job :id (format nil "~a-~a" unit v) :unit unit :variant v
                     :model (cdr (assoc tier *models*)) :wave wave)))
    (loop for spec in *units*
          append (if (consp spec)
                     (destructuring-bind (unit v . tier) spec (list (job unit v tier)))
                     (loop for (v . tier) in *variants* collect (job spec v tier))))))

(defun wave-units ()
  "The distinct units this wave touches (merge is per unit, not per arm)."
  (remove-duplicates (mapcar (lambda (s) (if (consp s) (first s) s)) *units*)
                     :test #'string= :from-end t))

;;; ---- scoring --------------------------------------------------------------

(defun restore-harness (wd)
  "Put the oracle and the swarm tooling back to HEAD before scoring."
  (sh "git -C ~a checkout -q HEAD -- ~{~a~^ ~} 2>/dev/null" wd *harness-paths*))

(defun keep-cmd (job)
  "What the oracle runs when it sees a new best: snapshot the WHOLE worktree as
   a patch.  `add -A -N' registers new files as intent-to-add so `diff HEAD'
   includes them; the temp-then-rename keeps a half-written patch from ever
   being restored."
  (format nil "git -C ~a add -A -N >/dev/null 2>&1; git -C ~a diff HEAD > ~a.tmp 2>/dev/null && mv ~a.tmp ~a"
          (jp job :wd) (jp job :wd) (jp job :patch) (jp job :patch) (jp job :patch)))

(defun score (job &key keep)
  "Run the oracle over JOB's worktree.  Returns (values total text).
   With :KEEP, the oracle also snapshots on every improvement."
  (let* ((wd (jp job :wd))
         (cmd (format nil
                      "cd ~a && XDG_CACHE_HOME=~a CL_SOURCE_REGISTRY='(:source-registry (:tree \"~a\") :ignore-inherited-configuration)' ~
                       WPT_ROOT=~a ORACLE_BESTS=~a ORACLE_EXPECTED=~a ORACLE_KEEP_SCORE=~a ~
                       ~:[~;~:*ORACLE_KEEP_CMD=~s ~]~
                       sbcl --dynamic-space-size 4096 --non-interactive ~
                       --load inspect/forms-oracle.lisp --eval '(weft.forms-oracle:run \"~a\")' 2>&1"
                      wd (jp job :cache) wd *wpt*
                      (jp job :bests) (jp job :expected) (jp job :score)
                      (and keep (keep-cmd job))
                      (job-unit job))))
    (restore-harness wd)
    ;; The cache is per-job and the worktree path is stable, so ASDF's fasls stay
    ;; valid between scoring runs.  The bash version wiped it every time and paid
    ;; a full weft rebuild (~60s) for each score, several times a round.
    (let ((out (sh "~a" cmd)))
      (spit (jp job :result) out)
      (values (total-of out) out))))

(defun best-score (job)
  (let ((s (slurp (jp job :score))))
    (when s (parse-integer s :junk-allowed t))))

(defun restore-best (job)
  "Reset the worktree to HEAD and re-apply the best patch.  A round killed
   mid-edit otherwise throws away everything the worker achieved earlier in it."
  (let ((wd (jp job :wd)) (patch (jp job :patch)))
    (sh "git -C ~a reset -q --hard HEAD && git -C ~a clean -qfd" wd wd)
    (when (and (probe-file patch) (plusp (or (ignore-errors
                                               (with-open-file (s patch) (file-length s)))
                                             0)))
      (sh-ok "git -C ~a apply ~a" wd patch))))

;;; ---- the task the worker is given ----------------------------------------

(defun guidance (unit)
  (cond
    ((string= unit "select")
     "HTMLSelectElement — options, selectedOptions, selectedIndex, value, length,
item/namedItem, add/remove, multiple, type, constraint validation.  Most of this
works; what is left is the selectedness rules (which <option> descendants count
as \"the list of options\" when nested inside <div>/<optgroup>/<select>, and
which one is selected by default), the placeholder-label-option validity rules,
and one test that times out.  Read the failing assertions carefully — each names
the exact nesting it expects to be excluded.")
    ((string= unit "textarea")
     "HTMLTextAreaElement — 25 of 27 pass.  The two that do not need
test_driver.send_keys (real WebDriver input) and form submission into an iframe;
both are probably out of reach.  Look for the cheaper wins elsewhere in the tree
if this unit will not move.")
    ((string= unit "meter")
     "HTMLMeterElement — currently 0 of 62.  value/min/max/low/high/optimum are
DOUBLE reflections, not string ones: each parses its content attribute as a
floating-point number and falls back to a default when the attribute is absent
or unparseable (value 0, min 0, max 1; low/high/optimum default to min/max/
midpoint).  Then the spec CLAMPS them against each other in a fixed order —
read the failing assertion messages, they walk you through it.  Setting the IDL
attribute writes the number back to the content attribute.  A leading '+' is
valid in the HTML number grammar.  A new src/script/forms-meter.lisp registered
with register-element-proto-extension is the natural home; add it to weft.asd.")
    ((string= unit "progress")
     "HTMLProgressElement — currently 0 of 16.  value/max are double reflections
with clamping (max defaults to 1 and must be > 0; value clamps to [0,max]),
position is -1 for an indeterminate progress (no value attribute) and
value/max otherwise, and labels comes from the shared labels machinery.  A new
src/script/forms-progress.lisp registered with register-element-proto-extension
is the natural home; add it to weft.asd.")
    ((string= unit "option")
     "HTMLOptionElement — 39 of 108.  The big one is option-text-spaces.html
(6/50): .text is the element's rendered-ish text with ASCII whitespace stripped
and collapsed, skipping <script>/<style> descendants — the whitespace rules are
the whole test.  Also: value falls back to text when there is no value
attribute, index within the owning select's list of options, selected vs the
selected content attribute (defaultSelected), form, label falling back to text,
and the Option(text, value, defaultSelected, selected) constructor.  Much of
this is already in src/script/forms-select.lisp — extend it there.")
    ((string= unit "fieldset")
     "HTMLFieldSetElement — 3 of 22.  elements (an HTMLFormControlsCollection of
the listed form controls in the fieldset), type (\"fieldset\"), name, form,
disabled — and the DISABLED FIELDSET rule that most of the tests are about: a
disabled <fieldset> disables all its descendant form controls EXCEPT those in
its first <legend> child.  That rule also drives willValidate/checkValidity,
which is why they are currently 0.  A new src/script/forms-fieldset.lisp
registered with register-element-proto-extension is the natural home; add it to
weft.asd.")
    ((string= unit "forminfra")
     "Form-control infrastructure — 3 of 120, and 98 of the misses are in ONE
file, form_attribute.html.  This is the `form' CONTENT ATTRIBUTE, and we honour
it nowhere.  Today the form owner is computed ad hoc as \"nearest ancestor
<form>\" in at least three places — forms-fieldset.lisp (fieldset-form-owner),
forms-select.lisp (the option \"form\" getter) and dom.lisp around line 2022.
The spec rule is: if a listed form-associated element has a `form' attribute,
its owner is the element in the same tree whose id matches, IF that element is a
<form> — and if nothing matches, it has NO owner even when nested inside a
<form>.  Only when the attribute is absent does the nearest-ancestor rule apply.
The association is LIVE: changing the attribute, moving either node, or changing
the target form's id re-resolves it, and form.elements must agree with
control.form in both directions.
Listed elements are button, fieldset, input, object, output, select, textarea.
The high-value move is one shared form-owner function that all the existing call
sites route through, rather than a fourth private copy — put it somewhere the
other feature files can reach it and fix the callers to use it.")
    ((string= unit "label")
     "HTMLLabelElement — 12 of 49.  htmlFor reflects the `for' content attribute.
`control' is the labeled control: with a `for' attribute it is the element in
the tree whose id matches, but ONLY if that element is labelable; with no `for'
attribute it is the FIRST labelable descendant in tree order.  Labelable =
button, input (except type=hidden), meter, output, progress, select, textarea.
`form' returns the control's form owner.  src/script/forms-labels.lisp already
implements the inverse direction (control.labels) — the labelable set and the
matching rules should be shared with it, not duplicated.
Ignore the click/focus behaviour: this unit's files are the IDL ones.  If a
failing assertion needs a real click to be dispatched, leave it.")
    ((string= unit "button")
     "HTMLButtonElement — 19 of 28, so this is the thin unit of the wave; take
the cheap ones and move on rather than grinding.  What is left: `type' is an
enumerated reflection limited to submit/reset/button with submit as both the
missing and the invalid default, matched ASCII case-insensitively; willValidate
is false for type=button and type=reset and for a disabled button (including one
disabled by an ancestor fieldset); plus labels, checkValidity,
setCustomValidity and validationMessage off the shared validity machinery in
src/script/forms-validity.lisp.  button-events.html and button-validation.html
hold most of the remaining subtests — read their failing assertions first.")
    (t "")))

(defun task-text (job)
  (let ((wd (jp job :wd)) (unit (job-unit job)))
    (format nil "~a

# Focused WPT swarm unit: <~a> element IDL  (variant ~a)

Implement HTMLElement IDL for <~a> in the weft engine (pure Common Lisp; JS on
the in-tree shuttle engine) so that more WPT subtests pass.

## Your working tree
  ~a
It is a git worktree.  **Edit any file in it you need to.**  Start from
src/script/forms-~a.lisp, but the remaining failures are not all there — when a
failure's cause is in src/script/dom.lisp, src/script/bridge.lisp or anywhere
else, fix it there.  Do not create files outside this tree.

The one exception: inspect/forms-oracle.lisp and tools/swarm/ are the harness
that scores you.  They are reset from git before every scoring run, so editing
them achieves nothing.

## The shared prototype is the main hazard — read this twice
Feature files install THEIR accessors onto the SAME element prototype, and
whoever installs last wins.  A generic name — value, type, length, labels,
checkValidity, willValidate, setCustomValidity — registered with the PLAIN
macros deletes a sibling's working implementation for every element, and a tag
guard whose else-branch returns js:*undefined* is exactly as destructive as no
guard at all.  Not theoretical: one run scored 6 -> 36 on its own unit while
destroying 131 subtests across five others, purely from js:*undefined*
else-branches.

So install with the -for variants, which gate on the tag AND delegate to
whatever was installed before.  Write only the <~a> case; there is no
else-branch to write.
  (defgetset-for ctx ep \"~a\" \"value\" (this) <getter> (v) <setter>)
  (defget-for    ctx ep \"~a\" \"type\"  (this) <getter>)
  (defmethod-for ctx ep \"~a\" \"add\" 2 (this a) <body>)
Use plain defmethod*/defget/defgetset only for a name nothing else could define.

## Oracle — run after EVERY edit
  ~a
It prints per-file pass/fail WITH THE NAME AND ASSERTION MESSAGE of every
failing subtest, the \"UNIT ~a: P passed, F failed\" line, a SENTINEL line per
OTHER unit, and finally:
  TOTAL <n> passed across 8 units, best-ever <b>
**TOTAL is your score.**  A gain paid for out of a sentinel is not a gain, and
any \"REGRESSION -n\" line means you have broken code you never read.  Run it
FIRST: it names exactly which subtests are broken and why, so you never have to
guess which of the files to read.

## ~a

## Helpers / rules
- Read ~a/src/script/dom.lisp (search \"defgetset ctx ep\") for the accessor
  style; read the sibling forms-*.lisp for validity / selection / reflection
  patterns; read src/script/bridge.lisp for how interfaces and prototypes are
  built.
- JS values: (num x) (jbool x) (jstr v) js:*true* js:*false* js:*null*
  js:*undefined*; live lists via (make-collection ctx (lambda () LIST) nil
  :nodelist); attrs via (get-attr node \"n\")/(set-attr node \"n\" v)/
  (dom:has-attribute node \"n\"); current value hash (context-input-values ctx);
  throw via (throw-dom ctx \"IndexSizeError\" 1 \"m\") or
  (js:js-throw (js:make-native-error \"TypeError\" \"m\")).
- PURE Common Lisp, NO regex/external libs.  Balanced parens; the oracle
  compiles first — fix any READ/compile error before logic.  Loop
  edit -> oracle -> fix, and keep going while TOTAL rises.
- NEVER hardcode the value a test asserts.  Returning the numbers a specific
  WPT file expects — a fixed rect from getBoundingClientRect, a canned string
  from a getter — scores points and is worthless: it is a fabricated value that
  no caller can tell from a computed one, and it will be reverted on review.  A
  method that must merely not throw may be an explicit no-op; a VALUE has to be
  derived from the document.  If you cannot derive it, leave the subtest failing
  and say so — an honest failure is worth more than a fake pass.
- Scratch and probe files: delete them before the round ends, or put them in
  /tmp.  Your WHOLE worktree diff is what gets merged, so a leftover diag.lisp
  at the repo root ships with your work.
"
            (or (slurp (format nil "~a/tools/swarm/API.md" *src*)) "")
            unit (job-variant job) unit
            wd unit
            unit unit unit unit
            ;; Same ORACLE_BESTS/ORACLE_EXPECTED as the harness uses, so the
            ;; agent's own oracle runs share one ratchet with ours AND keep the
            ;; oracle's bookkeeping out of the worktree — otherwise every
            ;; agent-run oracle rewrites forms-oracle-expected.sexp in place and
            ;; it rides along in the worker's patch as noise.
            (format nil "cd ~a && ORACLE_BESTS=~a ORACLE_EXPECTED=~a sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp --eval '(weft.forms-oracle:run \"~a\")' 2>&1 | tail -40"
                    wd (jp job :bests) (jp job :expected) unit)
            unit
            (guidance unit)
            wd)))

;;; ---- running one worker ---------------------------------------------------

(defun agent-command (job total)
  ;; The two OPERANDI_ vars are not decoration.  operandi's defaults are sized
  ;; for a small local model, and at the 24k context default these workers
  ;; thrashed so badly they made ZERO Write calls in 40 minutes across six arms
  ;; — an agent that looks lazy rather than one that looks broken.
  ;; `cd' and CL_SOURCE_REGISTRY are the two things that keep a worker inside its
  ;; own worktree, and wave 6 lost all three winning merges for want of them.
  ;; Without the `cd' the worker inherits the WAVE's cwd, which is the canonical
  ;; tree — so every relative-path edit it makes lands in canon.  And without the
  ;; registry any sbcl the worker starts itself resolves "weft" through
  ;; ~/quicklisp/local-projects/weft.asd, a symlink to the canonical tree, so its
  ;; edits appear to do nothing and it goes looking for the "real" file to fix.
  ;; Three of five workers found /home/claude/weft that way and wrote to it,
  ;; leaving canon dirty and making `git apply' fail for every arm that won.
  (format nil
          "cd ~a && CL_SOURCE_REGISTRY='(:source-registry (:tree \"~a\") :ignore-inherited-configuration)' ~
           OPERANDI_CONTEXT_BUDGET=~d OPERANDI_MAX_ITERS=~d ~
           timeout ~d sbcl --non-interactive --load ~a/bin/operandi.lisp -- ~
           --openrouter ~a --no-tools Fan,Task,Spawn ~s >> ~a 2>&1"
          (jp job :wd) (jp job :wd)
          *context-budget* *max-iters*
          *round-max* *operandi-root* (job-model job)
          (format nil "Read ~a and carry it out fully and autonomously. The tree ~
                       already scores TOTAL ~a; your job is to raise it. Run the ~
                       oracle FIRST — it names every failing subtest and the ~
                       assertion that failed, so you do not need to guess which ~
                       ones are broken. You may edit ANY file in the tree. Write ~
                       code early and often; do not spend the round reading. No ~
                       questions."
                  (jp job :task) (or total "?"))
          (jp job :log)))

(defun cost-of (job rounds)
  "operandi prints its usage line once per invocation, on exit; sum them.
   A round killed by `timeout' never reaches that print, so a capped wave has
   NO cost data at all.  Report that as `?' — awk's END block prints 0.000 on
   empty input, and dressing that up as `0.000¢' reads like a measurement."
  (let* ((n (sh "grep -acE '[0-9.]+¢' ~a 2>/dev/null" (jp job :log)))
         (lines (or (ignore-errors (parse-integer n :junk-allowed t)) 0)))
    (if (zerop lines)
        "cost ?"
        (let ((out (sh "grep -aoE '[0-9.]+¢' ~a 2>/dev/null | tr -d '¢' | ~
                        awk '{s+=$1} END{printf \"%.3f\", s}'"
                       (jp job :log))))
          ;; Still partial whenever a round was killed by `timeout': say so.
          (format nil "~a¢~:[ (~d/~d rounds)~;~]" out (= lines rounds) lines rounds)))))

(defun run-worker (job)
  (let* ((deadline (+ (get-universal-time) *worker-budget*))
         (start (get-universal-time))
         (round 0) (dry 0))
    (let ((total (score job :keep t)))
      (note "~a: start TOTAL ~a" (job-id job) (or total "?"))
      (loop
        (let ((left (- deadline (get-universal-time))))
          (when (< left 420)
            (note "~a: out of budget after ~d round~:p" (job-id job) round)
            (return))
          (when (>= dry *dry-rounds*) (return))
          (incf round)
          (sh "~a" (agent-command job total))
          (let ((after (score job :keep t))
                (best (best-score job)))
            ;; The oracle snapshots on every improvement, so a round that ended
            ;; below its own best is recoverable — take the patch back.
            (when (and best (< (or after 0) best))
              (restore-best job)
              (setf after (score job))
              (note "~a: round ~d restored best (TOTAL ~a)" (job-id job) round best))
            (if (> (or after 0) (or total 0)) (setf dry 0) (incf dry))
            (note "~a: round ~d TOTAL ~a -> ~a (dry=~d)"
                  (job-id job) round (or total "?") (or after "?") dry)
            (setf total after)))))
    (list :job job :rounds round :seconds (- (get-universal-time) start)
          :total (or (best-score job) 0) :cost (cost-of job round))))

;;; ---- merging --------------------------------------------------------------

(defun scripting-dom-ok-p (dir)
  "The regression gate that matters once workers may touch shared engine code:
   the forms oracle cannot see damage outside the forms tests."
  (let ((out (sh "cd ~a && sbcl --dynamic-space-size 4096 --non-interactive ~
                  --load inspect/scripting-dom.lisp 2>&1 | tail -3" dir)))
    (search "0 failed" out)))

(defun canon-score (unit)
  (sh "cd ~a && WPT_ROOT=~a sbcl --dynamic-space-size 4096 --non-interactive ~
       --load inspect/forms-oracle.lisp --eval '(weft.forms-oracle:run \"~a\")' 2>&1"
      *src* *wpt* unit))

(defun merge-best (results unit)
  "Apply the winning arm's WHOLE patch to the canonical tree, keep it only if
   TOTAL rises and scripting-dom stays green, revert otherwise."
  (let* ((arms (remove-if-not (lambda (r) (string= unit (job-unit (getf r :job))))
                              results))
         (winner (first (sort (copy-list arms) #'> :key (lambda (r) (getf r :total))))))
    (dolist (r arms)
      (note "  ~12a TOTAL ~a  [~a, ~ds, ~d round~:p, ~a]"
            (job-id (getf r :job)) (getf r :total)
            (car (last (uiop:split-string (job-model (getf r :job)) :separator "/")))
            (getf r :seconds) (getf r :rounds) (getf r :cost)))
    (unless winner (return-from merge-best nil))
    (let* ((job (getf winner :job))
           (patch (jp job :patch))
           (before (total-of (canon-score unit))))
      (unless (probe-file patch)
        (note "  -> no patch from ~a" (job-id job))
        (return-from merge-best nil))
      ;; An arm that never improved snapshots a zero-byte patch, and `git apply'
      ;; calls that "No valid patches in input" with exit 128 — which the failure
      ;; branch below would report as "does not apply cleanly", pointing at a
      ;; conflict that does not exist.  Name the empty case for what it is.
      (when (zerop (with-open-file (s patch) (file-length s)))
        (note "  -> ~a made no change; nothing to merge" (job-id job))
        (return-from merge-best nil))
      (note "  -> ~a touches: ~a" (job-id job)
            (or (sh "git -C ~a apply --numstat ~a 2>/dev/null | awk '{print $3}' | paste -sd,"
                    *src* patch)
                "?"))
      (multiple-value-bind (out code err) (sh "git -C ~a apply --3way ~a" *src* patch)
        (declare (ignore out))
        (if (not (zerop code))
          (note "  -> ~a patch failed to apply (git exit ~d): ~a"
                (job-id job) code (if (plusp (length err)) err "<no stderr>"))
          (let ((after (total-of (canon-score unit))))
            (cond ((and after before (> after before) (scripting-dom-ok-p *src*))
                   (note "  -> KEEP ~a: TOTAL ~a -> ~a" (job-id job) before after))
                  (t
                   ;; Reverse-apply THIS patch only.  A blanket checkout would
                   ;; also wipe an earlier unit's patch that we already kept.
                   (sh "git -C ~a apply -R --3way ~a" *src* patch)
                   (note "  -> REVERT ~a: TOTAL ~a -> ~a~:[~; (scripting-dom regressed)~]"
                         (job-id job) before after
                         (and after before (> after before)))))))))))

;;; ---- the wave -------------------------------------------------------------

(defun setup (job)
  (let ((wd (jp job :wd)))
    (sh "git -C ~a worktree add -q --detach --force ~a HEAD" *src* wd)
    (sh "rm -rf ~a" (jp job :cache))
    ;; The oracle's bookkeeping starts from the canonical tree's, but lives
    ;; outside the worktree so it never lands in the worker's diff.
    (sh "cp ~a/inspect/forms-oracle-bests.sexp ~a" *src* (jp job :bests))
    (sh "cp ~a/inspect/forms-oracle-expected.sexp ~a" *src* (jp job :expected))
    (spit (jp job :task) (task-text job))
    (spit (jp job :log) "")))

(defun wave (dir)
  (let ((dirty (sh "git -C ~a status --porcelain" *src*)))
    (when (plusp (length dirty))
      (format t "~&Refusing to run: ~a has uncommitted changes.~%~a~%~
                 The merge gate applies and reverts patches there; commit first.~%"
              *src* dirty)
      (return-from wave nil)))
  (sh "rm -rf ~a && mkdir -p ~a && git -C ~a worktree prune" dir dir *src*)
  (let ((jobs (make-jobs dir)))
    (mapc #'setup jobs)
    (note "[forms-elem-wave] ~d jobs in ~a, budget ~ds/worker" (length jobs) dir *worker-budget*)
    (let* ((threads (mapcar (lambda (j)
                              (sb-thread:make-thread #'run-worker :arguments (list j)
                                                     :name (job-id j)))
                            jobs))
           (results (mapcar #'sb-thread:join-thread threads)))
      (note "~&=== WAVE COMPLETE — merge ===")
      ;; The tree was clean when the wave started, so anything here now was put
      ;; there by a worker reaching outside its worktree.  It has to go before the
      ;; first apply: `git apply --3way' refuses any file that does not match the
      ;; index, so one stray edit to a shared file makes EVERY arm's patch fail,
      ;; and the reported reason ("does not match index") points nowhere near the
      ;; cause.  Say what was found — silently resetting would hide a worker bug.
      (let ((dirty (sh "git -C ~a status --porcelain" *src*)))
        (when (plusp (length dirty))
          (note "~&!! canon tree was polluted DURING the wave — a worker wrote outside~%~
                 !! its worktree.  Resetting before merge; the patches are unaffected.~%~a"
                dirty)
          (sh "git -C ~a checkout -- ." *src*)
          (sh "git -C ~a clean -fdq -- src inspect" *src*)))
      (dolist (unit (wave-units))
        (note "~a:" unit)
        (merge-best results unit))
      (note "~&Worktrees left in ~a for inspection; `git -C ~a worktree prune' after removing."
            dir *src*)
      results)))
