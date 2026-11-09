;;;; Native prerequisite controls for CASE-PROOF over synthetic contexts from
;;;; vttest-keyboard-native-prereq-fixtures.lisp. Load after
;;;; vttest-keyboard-driver-tests.lisp, vttest-keyboard-native-prereq.lisp and
;;;; vttest-keyboard-context-font.lisp. Entry point: (NATIVE-PREREQ-CONTROLS).
(in-package :vttest-keyboard-driver-tests)
(load (merge-pathnames "vttest-keyboard-native-prereq-fixtures.lisp" *load-truename*))
(unless (fboundp 'k::make-font-proof)
  (error "Native prerequisite controls need K::MAKE-FONT-PROOF loaded first"))

(defparameter *np-full-steps* '(:own-da :font :reply-end :targets :row :timing))

(defun np-steps (run) (mapcar #'car (getf run :trace)))

(defun np-event-out (ctx kind)
  "The :OUT of CTX's first event of KIND."
  (getf (find kind (k::case-context-events ctx) :key (lambda (e) (getf e :kind))) :out))

(defun np-stubbed ()
  (mapcar #'symbol-function '(k::load-case-context k::font-proof
                              vttest-keyboard-row-output:case-row-output-errors)))

(defun np-positive (label)
  (multiple-value-bind (ctx info) (np-case label)
    (let* ((run (np-run ctx)) (r (getf run :result))
           (row (first (getf run :rows))) (timing (getf run :timings))
           (first-key (np-event-out ctx :key-begin))
           (targets (mapcar (lambda (c) (getf c :out)) (np-captures ctx))))
      (flet ((ok (x tag) (check x (list label tag))))
        (ok (and (eq :pass (first r)) (null (second r)) (eq :pass (third r))
                 (null (fourth r)) (eq ctx (fifth r)))
            :pass-same-ctx)
        (ok (and (equal *np-full-steps* (np-steps run))
                 (every (lambda (s) (eq ctx (cdr s))) (getf run :trace)))
            :ordered-trace-same-ctx)
        (ok (and (equal (list (list label *np-dir*)) (getf run :loads)) (= 1 (getf run :fonts)))
            :one-load-one-font-call)
        (ok (and (equalp *np-octets* (k::case-context-oracle-octets ctx))
                 (not (eq *np-octets* (k::case-context-oracle-octets ctx))))
            :oracle-octets-copied)
        ;; The "menu" prefix puts the first key after the DA reply's position.
        (ok (and (> first-key (np-event-out ctx :enqueue)) (= first-key (getf info :reply-end)))
            :first-key-after-da-position)
        (ok (and row (= 1 (length (getf run :rows))) (equal label (first row))
                 (eq (k::case-context-output ctx) (second row))
                 (eql first-key (third row)) (equal targets (fourth row)))
            :row-gets-first-key-and-targets)
        (ok (equal (np-intervals info)
                   (loop for start = first-key then end for end in targets collect (cons start end)))
            :intervals-first-key-then-previous-target)
        (ok (and (= 1 (length timing))
                 (destructuring-bind (l d events output scale barriers) (first timing)
                   (and (equal label l) (equal *np-dir* d)
                        (eq (k::case-context-events ctx) events)
                        (eq (k::case-context-output ctx) output)
                        (eql k::+timing-scale+ scale)
                        (eq (k::case-context-barriers ctx) barriers))))
            :timing-gets-same-objects-and-barriers-keyword)))))

(defun np-expect (tag run state reason steps &key (fonts 1) (rows 0))
  "RUN stopped at the last of STEPS with STATE as both verdicts, REASON (unless NIL)
heading them, FONTS closure calls, ROWS row calls and no timing call."
  (let ((r (getf run :result)))
    (check (and (eq state (first r)) (eq state (third r)) (equal (second r) (fourth r))
                (or (null reason) (equal reason (first (second r))))
                (equal steps (np-steps run)) (= 1 (length (getf run :loads)))
                (= fonts (getf run :fonts)) (= rows (length (getf run :rows)))
                (null (getf run :timings)))
           tag)))

(defun np-stops (label)
  (let ((font-steps '(:own-da :font))
        (target-steps '(:own-da :font :reply-end :targets)))
    (flet ((stop (tag state reason steps run &rest keys)
             (apply #'np-expect (list label tag) run state reason steps keys))
           (fresh (&rest keys) (apply #'np-case label keys))
           (edited (fn) (let ((ctx (np-case label))) (funcall fn ctx) ctx)))
      (stop :bad-da :fail '(:own-da :reply-bytes) '(:own-da)
            (np-run (fresh :reply (format nil "~c[?1;0c" (code-char 27)))) :fonts 0)
      (stop :font-malformed-pass :fail '(:font :malformed-pass) font-steps
            (np-run (fresh) :font '(:pass nil nil)))
      (stop :font-unknown-state :fail nil font-steps (np-run (fresh) :font '(:maybe nil nil)))
      (stop :font-signal :fail '(:font (:signal simple-error)) font-steps
            (np-run (fresh) :font :signal))
      (stop :font-absent :unverified '(:font :no-font-consumer) font-steps
            (np-run (fresh) :font :absent) :fonts 0)
      (stop :font-explicit-unverified :unverified '(:font :unverified) font-steps
            (np-run (fresh) :font '(:unverified nil nil)))
      (stop :row-failure :fail '(:row :bad) (butlast *np-full-steps*)
            (np-run (fresh) :row '((:bad))) :rows 1)
      (stop :missing-barrier :unverified '(:targets :missing-barrier) target-steps
            (np-run (fresh :omit-barriers '(1))))
      (stop :mismatched-barrier :fail '(:targets :barrier-mismatch) target-steps
            (np-run (edited (lambda (ctx)
                              (setf (getf (cdr (first (k::case-context-barriers ctx))) :status)
                                    :stale)))))
      (stop :duplicate-barrier :fail '(:targets :barrier-inventory) target-steps
            (np-run (edited (lambda (ctx)
                              (push (copy-tree (first (k::case-context-barriers ctx)))
                                    (k::case-context-barriers ctx))))))
      ;; Empty first interval: the first target equals the first key position, which
      ;; would still exceed the DA position.
      (stop :empty-first-interval :fail '(:targets :target-order) target-steps
            (np-run (fresh :widths '(0))))
      (stop :nonincreasing-later-target :fail '(:targets :target-order) target-steps
            (np-run (fresh :widths '(4 0)))))))

(defun np-cache-control ()
  "One real MAKE-FONT-PROOF closure across a good case, a bad-DA case and another
good case, over a counted K::FONT-PROOF stub restored afterwards."
  (let ((calls 0) (old (symbol-function 'k::font-proof)) runs)
    (unwind-protect
         (progn
           (setf (symbol-function 'k::font-proof)
                 (lambda (transport root)
                   (declare (ignore transport root))
                   (incf calls)
                   (values :pass '() (copy-seq *np-octets*))))
           (let ((proof (k::make-font-proof '(:fixture t) *np-dir*))
                 (bad (format nil "~c[?1;0c" (code-char 27))))
             (setf runs (list (np-run (np-case (first *np-labels*)) :font proof)
                              (np-run (np-case (second *np-labels*) :reply bad) :font proof)
                              (np-run (np-case (second *np-labels*)) :font proof)))))
      (setf (symbol-function 'k::font-proof) old))
    (destructuring-bind (good bad again) runs
      (check (and (eq :pass (first (getf good :result))) (= 1 (getf good :fonts))
                  (eq :pass (first (getf again :result))) (= 1 (getf again :fonts)))
             :np-cache-good-cases-pass)
      (np-expect :np-cache-bad-da-second-case bad :fail '(:own-da :reply-bytes) '(:own-da)
                 :fonts 0)
      (check (= 1 calls) :np-cache-one-font-proof-call)
      (check (not (eq (k::case-context-oracle-octets (fifth (getf good :result)))
                      (k::case-context-oracle-octets (fifth (getf again :result)))))
             :np-cache-octets-fresh-per-case))))

(defun native-prereq-controls ()
  (let ((saved (np-stubbed)))
    (check (eq :disabled k::*proof-trace*) :np-proof-trace-default-disabled)
    (dolist (label *np-labels*)
      (np-positive label)
      (np-stops label))
    (np-cache-control)
    (check (and (eq :disabled k::*proof-trace*) (every #'eq saved (np-stubbed)))
           :np-stubs-and-trace-restored)))
