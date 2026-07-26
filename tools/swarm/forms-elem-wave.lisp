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

(defparameter *units*    '("select" "textarea"))
(defparameter *variants* '(("a" . :flash) ("b" . :flash) ("c" . :pro)))

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
  "Run a shell command line; return (values trimmed-stdout exit-code)."
  (multiple-value-bind (out err code)
      (uiop:run-program (apply #'format nil fmt args)
                        :output '(:string :stripped t)
                        :error-output nil
                        :ignore-error-status t)
    (declare (ignore err))
    (values out code)))

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
  (loop for unit in *units*
        append (loop for (v . tier) in *variants*
                     collect (make-job :id (format nil "~a-~a" unit v)
                                       :unit unit :variant v
                                       :model (cdr (assoc tier *models*))
                                       :wave wave))))

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
     "HTMLSelectElement — options (an HTMLOptionsCollection of <option> descendants),
selectedOptions, selectedIndex, value, length (get/set), item/namedItem,
add(element[,before]), remove([index]), multiple, type, the named/indexed getter,
and the constraint-validation members.  Several remaining failures are NOT in
forms-select.lisp: collections whose prototype is Object rather than
HTMLCollection, and indexed access returning undefined, both live in the DOM
bridge.  Go and fix them there.")
    ((string= unit "textarea")
     "HTMLTextAreaElement — value (raw/API value, defaulting to child text
content), defaultValue, textLength, type, cols, rows, wrap, maxLength/minLength,
setCustomValidity/validity/checkValidity, placeholder/readOnly/required.
Only four subtests remain; expect at least some of them to need changes outside
forms-textarea.lisp.")
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
"
            (or (slurp (format nil "~a/tools/swarm/API.md" *src*)) "")
            unit (job-variant job) unit
            wd unit
            unit unit unit unit
            (format nil "cd ~a && sbcl --dynamic-space-size 4096 --non-interactive --load inspect/forms-oracle.lisp --eval '(weft.forms-oracle:run \"~a\")' 2>&1 | tail -40"
                    wd unit)
            unit
            (guidance unit)
            wd)))

;;; ---- running one worker ---------------------------------------------------

(defun agent-command (job total)
  ;; The two OPERANDI_ vars are not decoration.  operandi's defaults are sized
  ;; for a small local model, and at the 24k context default these workers
  ;; thrashed so badly they made ZERO Write calls in 40 minutes across six arms
  ;; — an agent that looks lazy rather than one that looks broken.
  (format nil
          "OPERANDI_CONTEXT_BUDGET=~d OPERANDI_MAX_ITERS=~d ~
           timeout ~d sbcl --non-interactive --load ~a/bin/operandi.lisp -- ~
           --openrouter ~a --no-tools Fan,Task,Spawn ~s >> ~a 2>&1"
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

(defun cost-of (job)
  "operandi prints its usage line once per invocation, on exit; sum them.
   A round killed by `timeout' never reaches that print, so this under-reports —
   wave 4 read 0.000¢ across the board because every round hit the cap."
  (let ((out (sh "grep -aoE '[0-9.]+¢' ~a 2>/dev/null | tr -d '¢' | ~
                  awk '{s+=$1} END{printf \"%.3f\", s}'"
                 (jp job :log))))
    (if (and out (plusp (length out))) (format nil "~a¢" out) "?")))

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
          :total (or (best-score job) 0) :cost (cost-of job))))

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
      (note "  -> ~a touches: ~a" (job-id job)
            (or (sh "git -C ~a apply --numstat ~a 2>/dev/null | awk '{print $3}' | paste -sd,"
                    *src* patch)
                "?"))
      (if (not (sh-ok "git -C ~a apply --3way ~a" *src* patch))
          (note "  -> ~a patch does not apply cleanly; skipped" (job-id job))
          (let ((after (total-of (canon-score unit))))
            (cond ((and after before (> after before) (scripting-dom-ok-p *src*))
                   (note "  -> KEEP ~a: TOTAL ~a -> ~a" (job-id job) before after))
                  (t
                   ;; Reverse-apply THIS patch only.  A blanket checkout would
                   ;; also wipe an earlier unit's patch that we already kept.
                   (sh "git -C ~a apply -R --3way ~a" *src* patch)
                   (note "  -> REVERT ~a: TOTAL ~a -> ~a~:[~; (scripting-dom regressed)~]"
                         (job-id job) before after
                         (and after before (> after before))))))))))

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
      (dolist (unit *units*)
        (note "~a:" unit)
        (merge-best results unit))
      (note "~&Worktrees left in ~a for inspection; `git -C ~a worktree prune' after removing."
            dir *src*)
      results)))
