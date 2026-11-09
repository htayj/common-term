;;;; Focused font-pin admission tests for tests/vttest-vt52-pixels.lisp.
;;;; Run: .build/deps/bin/sbcl --no-userinit --no-sysinit --non-interactive \
;;;;        --load tests/vttest-vt52-font-pin-tests.lisp
(load (merge-pathnames "vttest-vt52-pixels.lisp" *load-truename*))

(defpackage #:vttest-vt52-font-pin-tests
  (:use #:cl #:vttest-vt52-pixels)
  (:local-nicknames (#:o #:vttest-vt52-oracle) (#:h #:vttest-vt52-sha256)))
(in-package #:vttest-vt52-font-pin-tests)

(defvar *checks* 0)
(defvar *failures* 0)
(defmacro check (name form)
  `(progn (incf *checks*)
          (unless (ignore-errors ,form)
            (incf *failures*) (format t "FAIL ~a~%" ,name))))

(defun octets (string) (map '(vector (unsigned-byte 8)) #'char-code string))
(check :sha-empty (equal (h:sha256-hex (octets ""))
                         "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
(check :sha-abc (equal (h:sha256-hex (octets "abc"))
                       "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
(check :sha-two-blocks
       (equal (h:sha256-hex
               (octets "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
              "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"))

(defparameter *font*
  (vttest-cursor-oracle:read-bytes
   (merge-pathnames "../.build/deps/share/fonts/terminus/ter-u16n.otb" *load-truename*)))
(defparameter *snapshot* (o:vt52-snapshot))

(defun raster (font claim)
  (expected-rgba *snapshot* font :font-sha256 claim :scale 1 :blink-phase t))
(defun refusal (font claim)
  "The refused field, or :ACCEPTED when a full frame is produced."
  (handler-case (and (= (* 640 384 4) (length (raster font claim))) :accepted)
    (pixel-refusal (c) (pixel-refusal-field c))))
(defun altered (index)
  (let ((font (copy-seq *font*)))
    (setf (aref font index) (logxor #xff (aref font index)))
    font))

(check :actual-font-is-pin (and (= +font-length+ (length *font*))
                                (equal +font-sha256+ (h:sha256-hex *font*))))
(check :actual-bytes-accepted (eq :accepted (refusal *font* +font-sha256+)))
(dolist (index (list 0 1000 (1- +font-length+)))
  (let ((font (altered index)))
    (check (list :altered-same-length index) (= (length font) (length *font*)))
    (check (list :altered-refused index) (eq :font (refusal font +font-sha256+)))))
(check :wrong-claim-refused (eq :font-sha256 (refusal *font* (h:sha256-hex (octets "abc")))))
(check :uppercase-claim-refused (eq :font-sha256 (refusal *font* (string-upcase +font-sha256+))))
(check :missing-claim-refused (eq :font-sha256 (refusal *font* nil)))
(check :short-font-refused (eq :font (refusal (subseq *font* 1) +font-sha256+)))
(check :non-octet-font-refused (eq :font (refusal (coerce *font* 'simple-vector) +font-sha256+)))

(format t "VT52 font pin tests: ~d checks, ~d failures (not native acceptance)~%"
        *checks* *failures*)
(unless (zerop *failures*) (sb-ext:exit :code 1))
