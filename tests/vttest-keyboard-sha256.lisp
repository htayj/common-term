;;;; Hash a file with the external sha256sum program, refusing anything
;;;; but one exact, well-formed digest line.

(require :asdf)

(defpackage #:vttest-keyboard-sha256
  (:use #:common-lisp)
  (:export #:sha256-file
           #:sha256-refusal
           #:sha256-refusal-path
           #:sha256-refusal-reason
           #:*sha256sum-program*
           #:*timeout-program*))

(in-package #:vttest-keyboard-sha256)

(defvar *sha256sum-program* "sha256sum"
  "Program run to hash a file; rebind it for negative controls.")

(defvar *timeout-program* "timeout"
  "Program bounding the hashing process's run time.")

(define-condition sha256-refusal (error)
  ((path :initarg :path :reader sha256-refusal-path)
   (reason :initarg :reason :reader sha256-refusal-reason))
  (:report (lambda (condition stream)
             (format stream "SHA-256 of ~A refused: ~A"
                     (sha256-refusal-path condition)
                     (sha256-refusal-reason condition)))))

(defun lowercase-hex-p (string)
  (every (lambda (char) (find char "0123456789abcdef")) string))

(defun sha256-file (path)
  "Return the SHA-256 digest of PATH as 64 lowercase hex characters."
  (let ((name (namestring path)))
    (flet ((refuse (format-control &rest arguments)
             (error 'sha256-refusal
                    :path name
                    :reason (apply #'format nil format-control arguments))))
      (multiple-value-bind (output error-output status)
          (handler-case
              (uiop:run-program (list *timeout-program* "--kill-after=1" "10"
                                      *sha256sum-program* "--" name)
                                :input nil
                                :output :string
                                :error-output :string
                                :ignore-error-status t)
            (error (condition)
              (refuse "running the process failed: ~A" condition)))
        (unless (eql status 0)
          (refuse "exit status ~S" status))
        (unless (string= error-output "")
          (refuse "unexpected stderr ~S" error-output))
        (unless (and (= (length output) (+ 64 2 (length name) 1))
                     (lowercase-hex-p (subseq output 0 64))
                     (string= output (format nil "  ~A~%" name) :start1 64))
          (refuse "unexpected stdout ~S" output))
        (subseq output 0 64)))))
