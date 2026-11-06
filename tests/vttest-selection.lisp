;;;; Pure recorder selection, also used for area-driver preflight.
(defpackage #:vttest-selection
  (:use #:cl)
  (:export #:select-cases #:invalid-selection))
(in-package #:vttest-selection)

(define-condition invalid-selection (error)
  ((reason :initarg :reason :reader selection-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid vttest selection: ~a" (selection-reason condition)))))

(defun reject-selection (reason)
  (error 'invalid-selection :reason reason))

(defun label-character-p (character)
  (or (char<= #\a character #\z) (char<= #\0 character #\9)
      (char= character #\-)))

(defun select-cases (cases &key exact-labels filter)
  "Return CASES selected for recording, without launching or reading anything.
EXACT-LABELS is NIL (absent) or a nonempty comma-separated string of unique,
case-sensitive known labels. Only ASCII lowercase letters, digits and hyphens
are allowed in a label. No whitespace, empty fields or alternate separators.
Return exact selections in requested order. Refuse any simultaneous non-NIL
FILTER, including an empty legacy filter. Reject ambiguous catalog entries.
With no exact selector, preserve the legacy substring FILTER and source order.
Call this function in an area driver's preflight before invoking the harness;
the recorder calls the same function before RUN-CASE. No Lisp reader is used."
  (if (null exact-labels)
      (remove-if-not (lambda (test) (or (null filter) (search filter (first test)))) cases)
      (progn
        (when filter (reject-selection "exact and legacy selectors cannot be combined"))
        (unless (and (stringp exact-labels) (<= 1 (length exact-labels) 4096))
          (reject-selection "exact selector must contain 1 to 4096 characters"))
        (let ((seen nil) (selected nil) (start 0))
          (loop
            (let* ((end (position #\, exact-labels :start start))
                   (label (subseq exact-labels start end)))
              (unless (and (plusp (length label)) (every #'label-character-p label))
                (reject-selection "empty or malformed exact label"))
              (when (member label seen :test #'string=)
                (reject-selection "duplicate exact label"))
              (let ((matches (remove-if-not (lambda (test) (equal label (first test))) cases)))
                (unless (= 1 (length matches))
                  (reject-selection "unknown or ambiguous exact label"))
                (push label seen)
                (push (first matches) selected))
              (unless end (return (nreverse selected)))
              (setf start (1+ end))))))))
