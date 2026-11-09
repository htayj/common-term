;;;; Pure fixtures for the capture-side blink proof. Standalone:
;;;; sbcl --non-interactive --load this file. No host, files or application.
(load (merge-pathnames "vttest-screen-blink-contract.lisp" *load-truename*))
(defpackage #:vttest-screen-blink-contract-tests
  (:use #:cl #:vttest-screen-blink-contract #:vttest-screen-blink-native))
(in-package #:vttest-screen-blink-contract-tests)

(defvar *checks* 0)
(defun check (name value)
  (incf *checks*)
  (format t "~S~%" (list :check name :pass (not (null value))))
  (unless value (error "Blink contract check failed: ~S" name)))

(defun gen (g &key (previous nil))
  "Status of lifetime toggle G, half a second apart, epoch 3 starting at 10."
  (list :id 1 :frames (+ 10 g) :bytes-received 500 :blink-phase (evenp g)
        :blink-visible t :blink-epoch 3 :blink-epoch-at 10 :blink-toggles g
        :blink-toggled-at (+ 10 (/ g 2))
        :blink-previous-at (or previous (+ 10 (/ (1- g) 2))) :blink-toggle-frame (+ 10 g)))
(defun gens (first count) (loop for g from first below (+ first count) collect (gen g)))
(defun with (status &rest pairs)
  (let ((copy (copy-tree status)))
    (loop for (k v) on pairs by #'cddr do (setf (getf copy k) v))
    copy))

(defun fixture (pause)
  "Valid original records for PAUSE as a plist of VALIDATE-BLINK-CAPTURE arguments."
  (let* ((last (case pause (13 5) (14 4) (t 6)))
         (ready (with (gen last) :frames (+ 11 last))))
    (list :selection (blink-selection-record pause (expected-blink-phase pause) 500
                                             (gens (1- last) 2))
          :chain (and (= pause 15) (blink-chain-record 15 (gens 1 2) (gens 2 4)))
          :ready ready :pre (copy-tree ready) :final (copy-tree ready) :target 500
          :comparison-phase (expected-blink-phase pause) :comparison-phase-present t)))
(defun run (pause &rest changes)
  (let ((args (fixture pause)))
    (loop for (k v) on changes by #'cddr do (setf (getf args k) v))
    (multiple-value-list (apply #'validate-blink-capture pause args))))
(defun ok-p (pause &rest changes) (first (apply #'run pause changes)))
(defun refused-p (error pause &rest changes)
  (let ((result (apply #'run pause changes)))
    (and (null (first result)) (assoc error (remove-if-not #'consp (second result))) t)))
(defun refused-flag-p (error pause &rest changes)
  (let ((result (apply #'run pause changes)))
    (and (null (first result)) (member error (second result)) t)))

(check :pause-13-hidden-valid (ok-p 13))
(check :pause-14-visible-valid (ok-p 14))
(check :pause-15-chain-valid (ok-p 15))
(check :explicit-nil-at-13 (null (getf (getf (fixture 13) :selection) :phase)))
(check :pause-12-refused (refused-flag-p :not-blink-pause 12 :selection nil))
(check :selection-missing (refused-p :selection 13 :selection nil))
(check :chain-missing (refused-p :chain 15 :chain nil))
(check :unexpected-chain (refused-flag-p :unexpected-chain 14
                                         :chain (blink-chain-record 15 (gens 1 2) (gens 2 4))))
(check :tampered-report
       (refused-p :chain 15 :chain (with (getf (fixture 15) :chain) :report '(:valid t))))
(check :tampered-chain-status
       (refused-p :chain 15 :chain (with (getf (fixture 15) :chain)
                                         :statuses (list (gen 2) (gen 3) (gen 4) (gen 6)))))
(check :tampered-selection-poll
       (refused-p :selection 14 :selection (with (getf (fixture 14) :selection)
                                                 :polls (list (gen 3) (with (gen 4) :id 2)))))
(check :wrong-selection-pause
       (refused-p :selection 13 :selection (getf (fixture 14) :selection)))
(check :selection-phase-omitted
       (refused-p :selection 13 :selection (list :kind :blink-selection :version 1 :pause 13
                                                 :target 500 :polls (gens 4 2))))
(check :wrong-selection-phase
       (refused-p :selection 14 :selection (with (getf (fixture 14) :selection) :phase nil)))
(check :wrong-target (refused-p :selection 14 :target 501))
(check :changed-ready (refused-p :capture 14 :ready (with (gen 4) :frames 16)))
(check :changed-pre (refused-p :capture 14 :pre (with (gen 4) :frames 16)))
(check :changed-final (refused-p :capture 13 :final (with (gen 5) :frames 17)))
(check :ready-not-linked (refused-p :selection 15 :ready (gen 7) :pre (gen 7) :final (gen 7)))
(check :source-epoch-changed
       (let ((r (with (gen 6) :frames 17 :blink-epoch 4)))
         (not (ok-p 15 :ready r :pre r :final r))))
(check :wrong-comparison-phase (refused-flag-p :comparison-phase-mismatch 14 :comparison-phase nil))
(check :wrong-comparison-phase-13 (refused-flag-p :comparison-phase-mismatch 13 :comparison-phase t))
(check :comparison-phase-key-missing
       (refused-flag-p :comparison-phase-missing 13 :comparison-phase-present nil))
(let ((cycle (list :id 1)))
  (setf (cdr (last cycle)) cycle)
  (check :circular-ready-refused (refused-p :malformed 14 :ready cycle))
  (check :circular-chain-refused (refused-p :chain 15 :chain cycle)))
(check :atom-pre-refused (refused-p :malformed 14 :pre 7))
(dolist (key '(:ready :pre :final))
  (check (list :dotted-status key) (not (ok-p 14 key (cons :id 1))))
  (check (list :oversized-status key)
         (not (ok-p 14 key (append (gen 4) (make-list 4100 :initial-element nil))))))
(dolist (key '(:selection :chain))
  (let ((shared (list :x)))
    (check (list :shared-sidecar key)
           (not (ok-p 15 key (list :a shared :b shared)))))
  (check (list :oversized-sidecar key)
         (not (ok-p 15 key (make-list 1048577 :initial-element nil))))
  (check (list :dotted-sidecar key) (not (ok-p 15 key (cons :a :b)))))
(let ((shared (list :x)))
  (check :shared-status-refused
         (not (ok-p 14 :pre (append (gen 4) (list :a shared :b shared))))))
(check :circular-selection-refused
       (let ((cycle (list :kind :blink-selection)))
         (setf (cdr (last cycle)) cycle)
         (not (ok-p 14 :selection cycle))))
(check :chain-after-selection-refused
       (not (ok-p 15 :chain (blink-chain-record 15 (gens 3 2) (gens 4 4)))))
(check :three-generations-refused
       (not (ok-p 15 :chain (blink-chain-record 15 (gens 1 2) (gens 2 3)))))
(check :crossed-phase-at-pre
       (not (ok-p 14 :pre (with (getf (fixture 14) :pre) :blink-phase nil))))
(check :crossed-phase-at-final
       (not (ok-p 13 :final (with (getf (fixture 13) :final) :blink-phase t))))
(check :stale-toggle-selection-refused
       (let ((args (fixture 14)))
         (not (ok-p 14 :selection
                    (with (getf args :selection)
                          :polls (list (gen 3) (with (gen 4) :blink-epoch-at 12)))))))
(check :long-finite-selection-valid
       (ok-p 14 :selection
             (blink-selection-record 14 t 500
               (append (loop repeat 150 collect (gen 3)) (list (gen 4))))))
(check :long-finite-chain-valid
       (ok-p 15 :chain
             (blink-chain-record 15 (gens 1 2)
               (append (loop repeat 150 collect (gen 2)) (gens 3 3)))))
(format t "~S~%" (list :blink-contract-checks *checks*))
