;;;; Pin half of vttest-keyboard-decoder: the external sha256sum pin and the in-memory
;;;; pinned decode. Loaded by vttest-keyboard-decoder.lisp after vttest-area.lisp.
(in-package #:vttest-keyboard-decoder)

(define-condition pin-refused (error)
  ((details :initarg :details :reader pin-refused-details))
  (:report (lambda (c s) (format s "Font SHA-256 pin refused: ~s" (pin-refused-details c)))))

(defun pinned-sha256 (path expected)
  "Return digest and command/exit/stdout/stderr details only if sha256sum exits 0,
stderr is empty, stdout is exactly DIGEST  PATH newline and DIGEST is EXPECTED.
Otherwise signal PIN-REFUSED with the details."
  (let ((command (list "sha256sum" "--" (namestring path))))
    (multiple-value-bind (out err exit)
        (uiop:run-program command :input nil :output :string :error-output :string
                                  :ignore-error-status t)
      (let* ((details (list :command command :exit exit :stdout out :stderr err
                            :expected expected))
             (digest (and (eql exit 0) (equal err "") (stringp out) (>= (length out) 64)
                          (subseq out 0 64))))
        (unless (and digest
                     (every (lambda (ch) (find ch "0123456789abcdef")) digest)
                     (string= out (format nil "~a  ~a~%" digest (namestring path)))
                     (string= digest expected))
          (error 'pin-refused :details details))
        (values digest details)))))

(defun decode-pinned (octets expected)
  "Hash OCTETS in memory. Only if the digest is EXPECTED, return a production font
parsed from one copy and oracle octets in another copy: EQUALP, never EQ."
  (let ((actual (vttest-area::sha256 octets)))
    (unless (equal actual expected)
      (error 'pin-refused
             :details (list :expected expected :actual actual :length (length octets))))
    (values (d::parse-otb (copy-seq octets)) (copy-seq octets))))

(defun load-pinned (path expected)
  "Read PATH once, then decode exactly those octets under the in-memory pin."
  (decode-pinned (o:read-bytes path) expected))
