;;;; Malformed blink proof evidence: READY and the stored chain report are checked for
;;;; finite shape before any field is read or compared, so atoms, dotted and circular
;;;; values are refused as data. Standalone: sbcl --non-interactive --load this file.
(load (merge-pathnames "vttest-screen-blink-native.lisp" *load-truename*))
(defpackage #:vttest-screen-blink-malformed-tests
  (:use #:cl #:vttest-screen-blink-native))
(in-package #:vttest-screen-blink-malformed-tests)

(setf *print-circle* t)
(defvar *checks* 0)
(defun check (name value)
  (incf *checks*)
  (format t "~S~%" (list :check name :pass (not (null value))))
  (unless value (error "Blink malformed check failed: ~S" name)))

(defun gen (g)
  (list :id 1 :frames (+ 10 g) :bytes-received 500 :blink-phase (evenp g)
        :blink-visible t :blink-epoch 3 :blink-epoch-at 10 :blink-toggles g
        :blink-toggled-at (+ 10 (* g 1/2)) :blink-previous-at (+ 10 (* (1- g) 1/2))
        :blink-toggle-frame (+ 10 g)))
(defun gens (first count) (loop for g from first below (+ first count) collect (gen g)))
(defun with (status &rest pairs)
  (let ((copy (copy-list status)))
    (loop for (k v) on pairs by #'cddr do (setf (getf copy k) v))
    copy))
(defun circular (&rest items) (let ((list (copy-list items))) (setf (cdr (last list)) list)))

(defparameter *chain* (blink-chain-record 15 (gens 1 2) (gens 2 4)))
(defparameter *selection* (blink-selection-record 15 t 500 (gens 5 2)))
(defparameter *ready* (with (gen 6) :frames 17))

(defun proof (&key (chain *chain*) (selection *selection*) (ready *ready*))
  "VALIDATE-BLINK-PROOF's values as a list, :SIGNALLED with the condition type, or
:TIMEOUT after five seconds."
  (handler-case (sb-ext:with-timeout 5
                  (multiple-value-list (validate-blink-proof 15 chain selection ready 500)))
    (sb-ext:timeout () :timeout)
    (error (c) (list :signalled (type-of c)))))
(defun refused (error &rest args)
  (let ((result (apply #'proof args)))
    (and (listp result) (null (first result)) (listp (second result))
         (member error (second result) :test #'equal) t)))

(defun ready-tests ()
  (dolist (case (list (list :ready-atom 42) (list :ready-dotted '(:id 1 . 2))
                      (list :ready-circular (circular :id 1 :bytes-received 500))))
    (check (first case) (refused :ready-malformed :ready (second case)))
    (check (list (first case) :no-target-claim)
           (not (refused :target-mismatch :ready (second case))))))

(defun report-tests ()
  (let* ((report (getf *chain* :report))
         (deep (list (list (circular 1 2))))
         (cases (list (list :report-atom 42)
                      (list :report-dotted (list* :valid t :errors nil 7))
                      (list :report-circular (apply #'circular report))
                      (list :report-deep-cycle (with report :errors deep)))))
    (dolist (case cases)
      (let ((chain (with *chain* :report (second case))))
        (check (first case) (refused :report-malformed :chain chain))
        (check (list (first case) :not-compared)
               (not (refused :report-not-recomputed :chain chain)))))
    (check :finite-tampered-still-compared
           (refused :report-not-recomputed :chain (with *chain* :report (with report :lateness nil))))))

(defun accepted-tests ()
  (check :canonical-proof-accepted (equal '(t nil) (proof)))
  (check :report-is-finite-plist (equal '(t nil) (proof :chain (copy-tree *chain*)))))

(defun status-tests ()
  (let ((bad (circular :id 1)))
    (check :circular-chain-status
           (refused :chain-invalid :chain (with *chain* :statuses (list (gen 2) bad))))
    (check :circular-selection-poll
           (refused '(:s1 (:selection (:malformed 1)))
                    :selection (blink-selection-record 15 t 500 (list (gen 5) bad))))
    (let ((c (handler-case
                 (sb-ext:with-timeout 5
                   (collect-blink-chain (let ((polls (list (gen 1) bad)))
                                          (lambda () (if (rest polls) (pop polls) (first polls))))
                                        :seconds 20 :sleep-fn (lambda ())))
               (blink-refusal (c) c)
               (sb-ext:timeout () nil))))
      (check :collector-refuses-malformed
             (and c (equal (blink-refusal-reason c) '((:statuses-malformed)))))
      (check :malformed-observation-kept
             (and c (eq bad (second (blink-refusal-observations c))))))))

(ready-tests)
(report-tests)
(accepted-tests)
(status-tests)
(format t "~S~%" (list :blink-malformed-tests :passed *checks*))
