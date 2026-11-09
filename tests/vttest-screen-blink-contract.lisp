;;;; Capture-side blink proof for the native contract checker. Pure: callers pass
;;;; the ORIGINAL records read back from the run root and every value is replayed,
;;;; never trusted. Pauses 13, 14 and 15 need the replayed selection linked to the
;;;; original READY, the strict READY/PRE/FINAL bracket at the pause's explicit
;;;; phase and an equal, explicitly present comparison phase. Pause 15 also needs
;;;; the chain proof: the chain precedes the selection, which precedes READY.
(load (merge-pathnames "vttest-screen-blink-native.lisp" *load-truename*))
(defpackage #:vttest-screen-blink-contract
  (:use #:cl #:vttest-screen-blink-native)
  (:import-from #:vttest-screen-blink-timing #:blink-capture-ready-p)
  (:import-from #:vttest-screen-blink-selection #:well-formed-p)
  (:export #:validate-blink-capture))
(in-package #:vttest-screen-blink-contract)

(defun validate-blink-capture (pause &key selection chain ready pre final target
                                          comparison-phase comparison-phase-present)
  "Validate blink PAUSE from its original SELECTION record, its CHAIN record (pause
15 only), the original READY, PRE and FINAL statuses, the request TARGET and the
comparison record's :PHASE value COMPARISON-PHASE; COMPARISON-PHASE-PRESENT is true
only when that record carries the :PHASE key. All three statuses must be well
formed before any selection, timing or chain function sees them.
Return T or NIL and the refusals."
  (let ((errors '()))
    ;; Bound sidecars before replay reaches any timing traversal or EQUAL.
    (dolist (entry (list (cons :selection selection) (cons :chain chain)))
      (unless (and (vttest-screen-blink-selection::finite-tree-p (cdr entry) 1048576)
                   (vttest-screen-blink-selection:plist-p (cdr entry)))
        (return-from validate-blink-capture
          (values nil (list (list (car entry) :malformed))))))
    (flet ((need (ok what) (unless ok (push what errors))))
      (multiple-value-bind (phase required) (expected-blink-phase pause)
        (need required :not-blink-pause)
        (need comparison-phase-present :comparison-phase-missing)
        (need (or (not comparison-phase-present) (eq phase comparison-phase))
              :comparison-phase-mismatch)
        (let ((shaped t))
          (loop for status in (list ready pre final) for name in '(:ready :pre :final)
                unless (well-formed-p status)
                  do (setf shaped nil) (need nil (list :malformed name)))
          (when (and required shaped)
            (multiple-value-bind (selected selection-errors)
                (validate-selection-record selection pause ready target)
              (need (and selected (ready-linked-p selected ready))
                    (cons :selection selection-errors)))
            (multiple-value-bind (ok capture-errors)
                (blink-capture-ready-p ready pre final :phase phase)
              (need ok (cons :capture capture-errors)))
            (if (eql pause 15)
                (multiple-value-bind (ok proof-errors)
                    (validate-blink-proof 15 chain selection ready target)
                  (need ok (cons :chain proof-errors)))
                (need (null chain) :unexpected-chain))))))
    (values (null errors) (nreverse errors))))
