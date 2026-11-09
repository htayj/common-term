;;;; Offline process controls for vttest-keyboard-sha256.
;;;; Load tests/vttest-keyboard-sha256.lisp first, then call RUN-TESTS.
;;;; Each run owns a fresh mktemp -d root, which it keeps as evidence.

(defpackage #:vttest-keyboard-sha256-tests
  (:use #:common-lisp #:vttest-keyboard-sha256)
  (:export #:run-tests))

(in-package #:vttest-keyboard-sha256-tests)

(defparameter *abc-digest*
  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  "SHA-256 of the three bytes abc (FIPS 180-2 example).")

(defvar *checks* 0)
(defvar *failures* '())
(defvar *root* nil "Private temporary directory owned by the current run.")

(defun check (name passed)
  (incf *checks*)
  (unless passed (push name *failures*))
  passed)

(defun run (&rest argv)
  (uiop:run-program argv :input nil :output :string :error-output :string))

(defun make-root ()
  (uiop:ensure-directory-pathname
   (string-right-trim '(#\Newline) (run "mktemp" "-d"))))

(defun write-fixture (name content &key executable)
  "Create NAME under *ROOT* exclusively, then read it back."
  (let ((path (merge-pathnames name *root*))
        (bytes (length (sb-ext:string-to-octets content :external-format :utf-8))))
    (assert (<= bytes 8192) () "Fixture ~A is ~D bytes" name bytes)
    (with-open-file (out path :direction :output :if-exists :error
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string content out))
    (when executable
      (run "chmod" "700" (namestring path)))
    (assert (string= content (uiop:read-file-string path :external-format :utf-8)))
    path))

(defun script (name body)
  (namestring (write-fixture name (format nil "#!/bin/sh~%~A~%" body)
                             :executable t)))

(defun expect-refusal (name path)
  "Check that hashing PATH signals a SHA256-REFUSAL naming PATH."
  (handler-case (progn (sha256-file path) (check name nil))
    (sha256-refusal (condition)
      (let ((reason (sha256-refusal-reason condition)))
        (check name (and (equal (sha256-refusal-path condition) (namestring path))
                         (stringp reason)
                         (plusp (length reason))))))
    (error () (check name nil))))

(defun digest-line (digest &key (suffix "") (tail ""))
  "Shell printing DIGEST, two spaces, the hashed name plus SUFFIX, a newline."
  (format nil "printf '%s  %s~A\\n' '~A' \"$2\"~A" suffix digest tail))

(defun check-fake-programs (data)
  (let ((cases
          `((:uppercase ,(digest-line (string-upcase *abc-digest*)))
            (:truncated ,(digest-line (subseq *abc-digest* 0 63)))
            (:long ,(digest-line (concatenate 'string *abc-digest* "0")))
            (:wrong-filename ,(digest-line *abc-digest* :suffix ".x"))
            (:extra-newline ,(digest-line *abc-digest* :suffix "\\n"))
            (:nonzero-exit ,(digest-line *abc-digest* :tail (format nil "~%exit 1")))
            (:stderr-status-0
             ,(digest-line *abc-digest* :tail (format nil "~%echo warning >&2"))))))
    (let ((*sha256sum-program* (script "fake-valid" (digest-line *abc-digest*))))
      (check :fake-valid-accepted (equal *abc-digest* (ignore-errors (sha256-file data)))))
    (loop for (name body) in cases
          do (let ((*sha256sum-program*
                     (script (format nil "fake-~(~A~)" name) body)))
               (expect-refusal name data)))))

(defun check-argv (data)
  "A recording timeout fixture sees the exact argv, spaces intact."
  (let* ((log (merge-pathnames "argv.log" *root*))
         (*timeout-program*
           (script "recording-timeout"
                   (format nil "set -C~%printf '%s\\n' \"$@\" > '~A'~%shift 2~%exec \"$@\""
                           (namestring log))))
         (digest (ignore-errors (sha256-file data))))
    (check :argv-digest (equal *abc-digest* digest))
    (check :argv-exact
           (and (probe-file log)
                (string= (uiop:read-file-string log)
                         (format nil "--kill-after=1~%10~%sha256sum~%--~%~A~%"
                                 (namestring data)))))))

(defun check-process-failures (data)
  (let ((*sha256sum-program* (namestring (merge-pathnames "missing-sha256sum" *root*))))
    (expect-refusal :missing-digest-program data))
  (let ((*timeout-program* (namestring (merge-pathnames "missing-timeout" *root*))))
    (expect-refusal :missing-timeout-program data))
  ;; Controlled stand-in: real output followed by timeout's status 124.
  ;; This shows status 124 is refused; it does not exercise an actual kill.
  (let ((*timeout-program* (script "status-124-timeout"
                                   (format nil "shift 2~%\"$@\"~%exit 124"))))
    (expect-refusal :timeout-status-124 data)))

(defun run-tests ()
  "Run the controls; return success, the check count and failed names."
  (let ((*checks* 0) (*failures* '()) (*root* (make-root)))
    (assert (not (find #\' (namestring *root*))))
    (let ((data (write-fixture "a b.txt" "abc")))
      (check :real-digest (equal *abc-digest* (ignore-errors (sha256-file data))))
      (check-argv data)
      (check-fake-programs data)
      (check-process-failures data))
    (format t "~&~D checks, ~D failed ~S; evidence kept in ~A~%"
            *checks* (length *failures*) (reverse *failures*) *root*)
    (values (null *failures*) *checks* (reverse *failures*))))
