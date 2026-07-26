;;;; rebase-merge.lisp — merge a finished wave best-first, rebasing conflicts.
;;;;
;;;; Load AFTER tools/swarm/forms-elem-wave.lisp; operates on a wave directory
;;;; that has already run, reading each arm's score/patch off disk (so it works
;;;; post-hoc, from a different process than the one that ran the wave).
;;;;
;;;; Why this exists.  `merge-best' merges per UNIT, applies the winner's patch
;;;; with `git apply --3way', and on conflict throws that arm away.  Two things
;;;; are wrong with that once units stop being disjoint:
;;;;
;;;;   1. The collision is on a FILE, not a unit.  Wave 8 named two units at one
;;;;      subsystem, and both arms rewrote the same pre-wired stub whole.  In
;;;;      *units* order the weaker patch (TOTAL 1289) defines the file and the
;;;;      stronger one (1465) is then a conflict — so the merge keeps the worse
;;;;      arm and both log lines read like successes.
;;;;
;;;;   2. A conflicting patch is not worthless.  It is a branch that lost the
;;;;      race.  Nobody deletes their work when main moves under them; they
;;;;      rebase and resolve.  A textual 3-way cannot do that here — when both
;;;;      sides rewrote one stub from scratch there is no common structure to
;;;;      merge — but a reader that understands both versions can, and the agent
;;;;      that wrote the losing patch is exactly such a reader.
;;;;
;;;; So: sort arms by TOTAL descending, apply what applies, and hand anything
;;;; that conflicts to a fresh agent along with the merged tree and its own
;;;; superseded diff.  The oracle gates the result the same as any other round,
;;;; and the floor is the TOTAL the merge already reached — a rebase may only
;;;; add.

(in-package #:weft.swarm)

(defparameter *rebase-budget* 1800
  "Seconds one rebase agent may run.  A rebase reads two versions of one file
   and ports the difference; it is a smaller job than a discovery round.")

(defun unit-counts (text)
  "Alist of UNIT-NAME -> subtests passed, read off one oracle run.  The oracle
   reports every unit it did not focus on as `SENTINEL <name> <n> passed (best
   <b>)' and the focused one as `UNIT <name>: <n> passed, ...', so a single run
   yields the whole per-unit profile."
  (let (out)
    (dolist (line (lines-starting text "SENTINEL "))
      (let* ((rest (string-left-trim " " (subseq line 9)))
             (sp (position #\Space rest)))
        (when sp
          (push (cons (subseq rest 0 sp)
                      (or (parse-integer rest :start sp :junk-allowed t) 0))
                out))))
    (dolist (line (lines-starting text "UNIT "))
      (let* ((rest (subseq line 5))
             (colon (position #\: rest)))
        (when colon
          (push (cons (subseq rest 0 colon)
                      (or (parse-integer rest :start (1+ colon) :junk-allowed t) 0))
                out))))
    (nreverse out)))

(defun canon-profile ()
  "Canon's TOTAL and its per-unit counts, from ONE oracle run.  Any unit name
   scores it — the TOTAL line is over all units — so use the first one the wave
   touched."
  (let ((text (canon-score (first (wave-units)))))
    (values (total-of text) (unit-counts text))))

(defun canon-total ()
  (values (canon-profile)))

(defun unit-regressions (before after)
  "Units whose count FELL between two profiles, as ((name before after) ...).
   TOTAL rising is not enough of a gate.  A patch that wins 40 subtests in its
   own unit while quietly costing 20 in a sibling still raises TOTAL, and the
   merge keeps it; the loss only surfaces rounds later as an unexplained
   sentinel gap that nobody can attribute to a patch any more."
  (loop for (unit . was) in before
        for now = (cdr (assoc unit after :test #'string=))
        when (and now (< now was)) collect (list unit was now)))

(defun accumulated-diff (path)
  "Everything the merge has kept so far, as a patch against HEAD.  The oracle's
   own bookkeeping files are excluded: they are auto-maintained, and letting one
   ride along in a patch is how wave 7 lost an arm."
  (sh "git -C ~a add -A -N >/dev/null 2>&1; ~
       git -C ~a diff HEAD -- . ':(exclude)inspect/forms-oracle-*.sexp' > ~a"
      *src* *src* path)
  path)

(defun disk-results (wave)
  "Reconstruct the wave's results from its artefacts, newest score on disk.
   `run-worker' returns these in-image, but a merge run afterwards — from a
   different process, or after a crash — only has the directory."
  (loop for job in (make-jobs wave)
        for raw = (slurp (jp job :score))
        collect (list :job job
                      :total (or (and raw (parse-integer raw :junk-allowed t)) 0))))

(defun orphan-check (dir)
  "Delete any NEW src/ file the .asd files never name, and say so.

   An agent that writes src/script/autocomplete-helper.lisp, decides against it
   and never wires it into weft.asd leaves a file that compiles nowhere and
   changes nothing — but it is in the patch, so it lands in canon, and the NEXT
   patch to touch that path fails with `already exists in working directory'.
   That is what rejected an otherwise good rebase in wave 8; the arm was blamed
   for a dead file it had abandoned."
  (let* ((manifest (sh "cat ~a/*.asd" dir))
         (new (remove "" (uiop:split-string
                          (or (sh "git -C ~a ls-files --others --exclude-standard -- src" dir) "")
                          :separator '(#\Newline))
                      :test #'string=)))
    (dolist (path new)
      (let ((stem (pathname-name path)))
        (unless (and stem (search stem manifest))
          (note "  -> orphan ~a (no .asd references it); removing" path)
          (sh "rm -f ~a/~a" dir path))))))

;;; ---- the rebase task ------------------------------------------------------

(defun oracle-command-line (wd job)
  "The exact shell line the worker should run to score its tree."
  (format nil
          "cd ~a && XDG_CACHE_HOME=~a WPT_ROOT=~a ORACLE_BESTS=~a ORACLE_EXPECTED=~a ~
           sbcl --dynamic-space-size 4096 --non-interactive ~
           --load inspect/forms-oracle.lisp --eval '(weft.forms-oracle:run \"~a\")'"
          wd (jp job :cache) *wpt* (jp job :bests) (jp job :expected) (job-unit job)))

(defun rebase-task-text (job floor)
  (format nil
"# Rebase your branch onto the merged tree

Another agent solved part of the same subsystem you did, and its patch reached a
higher score, so it went into the tree first. Your diff no longer applies. This
is an ordinary rebase: the work is not thrown away, it is carried across.

The tree you are sitting in ALREADY CONTAINS the other agent's solution and
scores TOTAL ~d. That number is a floor. Finishing below it means the rebase is
discarded, so the safe move is additive: keep what is here working, and bring
across only what it is missing.

Your superseded diff is at:

    ~a

It was cut against the older tree, so do not try to apply it — read it. Some of
it will already be present in different words; that part is done, drop it.
Some of it will cover cases the other version misses; that part is why this
step exists.

The merged tree is COMMITTED as this worktree's HEAD, so git behaves normally:
`git diff` shows your rebase and nothing else, `git checkout -- <file>` throws
away your change to that file and gives you the merged version back, and
`git stash` round-trips. Only what you add on top of HEAD is taken.

Start by running the oracle to see where the merged tree still fails:

    ~a

Then work the failures. You may edit any file. The oracle names every failing
subtest and the assertion text, so you do not have to guess.

Two things that are easy to get wrong here:

- Do not restore your version of a function wholesale because it looks more
  familiar. If the merged tree passes a test yours did not, replacing it loses
  that test and the oracle will tell you only after the fact.
- If both versions define the same name, the file has one definition, not two.
  Reconcile them into one that satisfies both sets of subtests.

Stop when the oracle stops rising. Leave no scratch files in the tree."
          floor
          (jp job :orig)
          (oracle-command-line (jp job :wd) job)))

;;; ---- the rebase itself ----------------------------------------------------

(defun rebase-arm (loser floor)
  "Give LOSER's superseded patch to a fresh agent working on a copy of the
   merged tree.  Returns the resulting TOTAL if it beat FLOOR and canon was
   updated, else NIL with canon untouched."
  (let* ((wave (job-wave loser))
         (job (make-job :id (format nil "~a-rebase" (job-id loser))
                        :unit (job-unit loser) :variant "r"
                        :model (job-model loser) :wave wave))
         (wd (jp job :wd)))
    (note "  -> REBASE ~a onto the merged tree (floor TOTAL ~a)" (job-id loser) floor)
    ;; A worktree at HEAD plus the accumulated diff IS the merged tree.  The
    ;; merge keeps patches in canon's WORKING tree rather than committing them,
    ;; so a plain worktree at HEAD would not have them.
    (sh "git -C ~a worktree add -q --detach --force ~a HEAD" *src* wd)
    (let ((merged (format nil "~a.merged" (jp job :wd))))
      (accumulated-diff merged)
      (when (plusp (or (ignore-errors (with-open-file (s merged) (file-length s))) 0))
        (unless (sh-ok "git -C ~a apply ~a" wd merged)
          (note "  -> could not stage the merged tree for the rebase; skipping")
          (sh "git -C ~a worktree remove --force ~a" *src* wd)
          (return-from rebase-arm nil))))
    ;; COMMIT the merged state before the agent touches anything.  Left as
    ;; working-tree changes it is one `git checkout -- <file>' away from being
    ;; destroyed, and that is not an exotic mistake: it is what everyone types to
    ;; undo an edit.  The first rebase agent did exactly that, reverted
    ;; forms-constraints.lisp to the pre-merge stub, and the engine stopped
    ;; compiling — the oracle scored TOTAL 0 and the rebase was "dropped for no
    ;; gain", blaming the agent for a hole in the harness.  Committed, the same
    ;; command restores the merged version, `git diff' shows only the rebase, and
    ;; `git stash' round-trips.  The worker's ordinary git reflexes have to be
    ;; safe by construction, exactly like the `cd' and CL_SOURCE_REGISTRY pinning
    ;; that keeps it out of canon.
    ;; `add -A', not `commit -a': the latter stages modified TRACKED files only,
    ;; so a file an earlier merge introduced would stay untracked in the base and
    ;; reappear in this rebase's delta as though this agent had written it.
    (sh "git -C ~a add -A" wd)
    (sh "git -C ~a -c user.name=swarm -c user.email=swarm@local ~
         commit -q -m 'merged state: the base this rebase starts from' ~
         --allow-empty" wd)
    (sh "cp ~a/inspect/forms-oracle-bests.sexp ~a" *src* (jp job :bests))
    (sh "cp ~a/inspect/forms-oracle-expected.sexp ~a" *src* (jp job :expected))
    (sh "cp ~a ~a" (jp loser :patch) (jp job :orig))
    (spit (jp job :task) (rebase-task-text job floor))
    (spit (jp job :log) "")
    (let ((*round-max* *rebase-budget*))
      (sh "~a" (agent-command job floor)))
    (let ((after (score job)))
      (cond
        ((and after (> after floor))
         (let ((out (jp job :patch)))
           ;; HEAD in the rebase worktree is the merged-state commit, so this
           ;; diff is the rebase DELTA alone — nothing of the merge is in it.
           ;; Canon already holds the merged state, so the delta applies straight
           ;; on top; no reset, no laying down a whole tree.
           (sh "git -C ~a add -A -N >/dev/null 2>&1; ~
                git -C ~a diff HEAD -- src > ~a" wd wd out)
           (if (sh-ok "git -C ~a apply ~a" *src* out)
               (progn (note "  -> REBASE KEPT ~a: TOTAL ~a -> ~a" (job-id loser) floor after)
                      after)
               (progn (note "  -> rebase produced a patch canon rejects; canon left at merged state")
                      nil))))
        (t (note "  -> REBASE DROPPED ~a: TOTAL ~a -> ~a (no gain over the merged tree)"
                 (job-id loser) floor (or after "?"))
           nil)))))

;;; ---- the driver -----------------------------------------------------------

(defun merge-wave (wave)
  "Merge every arm of a finished WAVE, best first, rebasing what conflicts.
   Ordering by TOTAL rather than by unit is the point: when two arms rewrote one
   file, the stronger must define it and the weaker must rebase onto it."
  (let ((results (sort (remove-if (lambda (r) (zerop (getf r :total)))
                                  (disk-results wave))
                       #'> :key (lambda (r) (getf r :total)))))
    (note "~&=== merge-wave ~a: ~d arm~:p, best first ===" wave (length results))
    (dolist (r results)
      (let* ((job (getf r :job))
             (patch (jp job :patch)))
        (note "~a (arm TOTAL ~a):" (job-id job) (getf r :total))
        (cond
          ((not (probe-file patch)) (note "  -> no patch"))
          ((zerop (with-open-file (s patch) (file-length s)))
           (note "  -> made no change; nothing to merge"))
          (t
           (multiple-value-bind (before before-units) (canon-profile)
            (let ((snap (format nil "~a.pre" patch)))
             (accumulated-diff snap)
             (note "  -> touches: ~a"
                   (or (sh "git -C ~a apply --numstat ~a 2>/dev/null | awk '{print $3}' | paste -sd,"
                           *src* patch)
                       "?"))
             (flet ((restore ()
                      (sh "git -C ~a reset -q --hard HEAD" *src*)
                      (sh "git -C ~a clean -fdq -- src inspect" *src*)
                      (when (plusp (or (ignore-errors
                                         (with-open-file (s snap) (file-length s)))
                                       0))
                        (sh "git -C ~a apply ~a" *src* snap))))
               (multiple-value-bind (out code err) (sh "git -C ~a apply --3way ~a" *src* patch)
                 (declare (ignore out))
                 (cond
                   ;; Conflict is not failure — it means main moved.  Rebase.
                   ((not (zerop code))
                    (note "  -> does not apply (git exit ~d): ~a" code
                          (if (plusp (length err)) err "<no stderr>"))
                    (restore)
                    (rebase-arm job before))
                   (t
                    (orphan-check *src*)
                    (multiple-value-bind (after after-units) (canon-profile)
                      (let ((lost (unit-regressions before-units after-units)))
                        (cond
                          ((and after before (> after before) (null lost)
                                (scripting-dom-ok-p *src*))
                           (note "  -> KEEP ~a: TOTAL ~a -> ~a" (job-id job) before after))
                          ;; A net gain that costs a sibling unit is still a
                          ;; conflict — the arm solved its own unit on top of
                          ;; something another unit needed.  Same remedy as a
                          ;; textual conflict: rebase onto what is already here.
                          ((and after before (> after before) lost)
                           (note "  -> ~a raises TOTAL ~a -> ~a but costs~{ ~{~a ~a->~a~}~}"
                                 (job-id job) before after lost)
                           (restore)
                           (rebase-arm job before))
                          ;; Applied cleanly but bought nothing: usually means an
                          ;; earlier arm already covered it.  Not a rebase case.
                          (t (restore)
                             (note "  -> REVERT ~a: TOTAL ~a -> ~a~:[~; (scripting-dom regressed)~]"
                                   (job-id job) before after
                                   (and after before (> after before))))))))))))))))
      (finish-output))
    (note "~&=== canon TOTAL after merge: ~a ===" (canon-total))))
