;;;; Fixtures for vttest-keyboard-context-font-tests.lisp. Every reader, verifier and
;;;; decoder FONT-PROOF reaches becomes a counted stub or a tripwire, always restored.
;;;; Stubbed control coverage only: the real pinned decode is proven by the decoder RUN.
;;;; Load after vttest-keyboard-driver-tests.lisp and vttest-keyboard-context-font.lisp.
(in-package :vttest-keyboard-driver-tests)

(defparameter *font-io*
  '((:verify . vttest-keyboard-font-evidence:verify-font-evidence)
    (:read . vttest-keyboard-font-evidence::read-regular)
    (:decode . vttest-keyboard-decoder:decode-pinned)
    (:compare . vttest-keyboard-decoder:compare-codes)
    (:font-path . vttest-keyboard-decoder:font-path)
    (:directory . k::check-font-directory))
  "Key and function of everything FONT-PROOF reaches that reads, verifies or decodes.")
(defparameter *fixture-truename* (namestring (truename *load-truename*))
  "An existing file standing in for the pinned font truename.")
(defparameter *font-root* "/run/vttest-font-control/")
(defparameter *good-title* "Lispterm-vttest-0123456789abcdef-4242")

(define-condition font-tripwire (error) ((key :initarg :key :reader tripwire-key)))
(defvar *font-calls* nil "Key -> argument lists of its calls, newest first.")

(defun font-calls (key) (length (gethash key *font-calls*)))
(defun font-args (key) (first (gethash key *font-calls*)))
(defun font-io-calls () (loop for (key) in *font-io* sum (font-calls key)))
(defun font-functions () (mapcar (lambda (e) (symbol-function (cdr e))) *font-io*))

(defun call-in-font-world (overrides thunk)
  "Run THUNK with every *FONT-IO* function counted: the last OVERRIDES plist entry for
its key, else a FONT-TRIPWIRE. Always restores the originals."
  (let ((old (font-functions)) (*font-calls* (make-hash-table)))
    (unwind-protect
         (progn
           (loop for (key . symbol) in *font-io*
                 for fn = (loop with f for (k g) on overrides by #'cddr
                                when (eq k key) do (setf f g) finally (return f))
                 do (setf (symbol-function symbol)
                          (let ((key key) (fn fn))
                            (lambda (&rest args)
                              (push args (gethash key *font-calls*))
                              (if fn (apply fn args) (error 'font-tripwire :key key))))))
           (funcall thunk))
      (loop for (nil . symbol) in *font-io* for fn in old
            do (setf (symbol-function symbol) fn)))))

(defmacro in-font-world (overrides &body body)
  `(call-in-font-world ,overrides (lambda () ,@body)))

(defun font-returning (&rest values)
  (lambda (&rest args) (declare (ignore args)) (values-list values)))
(defun font-signalling (type &rest initargs)
  (lambda (&rest args) (declare (ignore args)) (apply #'error type initargs)))

(defun font-snapshot ()
  (make-array vttest-keyboard-font-evidence::*pinned-length*
              :element-type '(unsigned-byte 8) :initial-element 7))
(defun flipped (octets)
  (let ((copy (copy-seq octets))) (setf (aref copy 0) (logxor 1 (aref copy 0))) copy))
(defun ascii-codes () (loop for code from 32 to 126 collect code))
(defun wrong-sha () (concatenate 'string "1" (subseq vttest-keyboard-decoder:*font-sha256* 1)))

(defun font-transport (&key (root *font-root*) (title *good-title*))
  (list :directory (concatenate 'string root "font-evidence/")
        :pinned-truename *fixture-truename* :expected-title title))
(defun font-verdict (length &key (sha vttest-keyboard-decoder:*font-sha256*))
  (list :claim :loaded-octets-equal-pinned :truename *fixture-truename*
        :sha256 sha :length length))

(defun pass-world (snapshot &key (codes (ascii-codes)) (verdict (font-verdict (length snapshot)))
                              (font-bytes snapshot) (oracle snapshot))
  "Overrides under which FONT-PROOF passes on SNAPSHOT; each keyword perturbs one stage.
:DECODE returns a font over a copy of FONT-BYTES and a copy of ORACLE."
  (list :directory (font-returning nil)
        :font-path (font-returning *fixture-truename*)
        :verify (font-returning verdict nil)
        :read (font-returning snapshot)
        :decode (lambda (octets sha)
                  (declare (ignore octets sha))
                  (values (lispterm.display::make-bitmap-font :bytes (copy-seq font-bytes))
                          (copy-seq oracle)))
        :compare (font-returning (mapcar (lambda (code) (cons code :match)) codes))))

(defun proof-values (transport root)
  "Values of the first call of a fresh MAKE-FONT-PROOF closure, as a list."
  (multiple-value-list (funcall (k::make-font-proof transport root))))

(defun load-font (overrides)
  "LOAD-PROVEN-FONT on *FIXTURE-TRUENAME* under OVERRIDES. Values: its values as a list
or its refusal as (STATE . REASON), :READ arguments, :READ and :DECODE calls, all I/O
calls and :DECODE arguments."
  (in-font-world overrides
    (values (handler-case (multiple-value-list (k::load-proven-font *fixture-truename*))
              (k::font-proof-refusal (c) (cons (k::font-proof-state c) (k::font-proof-reason c))))
            (font-args :read) (font-calls :read) (font-calls :decode) (font-io-calls)
            (font-args :decode))))
