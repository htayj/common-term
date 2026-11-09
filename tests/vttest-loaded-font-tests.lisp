;;;; Focused controls for optional loaded-font evidence recording.

(require "asdf")
(require "sb-posix")

(defpackage #:lispterm.loaded-font-tests (:use #:cl))
(in-package #:lispterm.loaded-font-tests)

(defvar *source* (merge-pathnames "../src/display/" *load-truename*))
(dolist (name '("package" "font-otb" "font"))
  (load (merge-pathnames (format nil "~a.lisp" name) *source*)))

(defvar *failures* '())
(defvar *checks* 0)

(defmacro check (name form)
  `(progn (incf *checks*)
          (unless (ignore-errors ,form) (push ,name *failures*))))

(defun fixture-octets ()
  (let ((v (make-array 160 :element-type '(unsigned-byte 8) :initial-element 0)))
    (labels ((u16 (p n)
               (setf (aref v p) (ldb (byte 8 8) n) (aref v (1+ p)) (ldb (byte 8 0) n)))
             (u32 (p n) (u16 p (ldb (byte 16 16) n)) (u16 (+ p 2) (ldb (byte 16 0) n)))
             (table (i tag offset)
               (let ((p (+ 12 (* i 16))))
                 (loop for c across tag for k from p do (setf (aref v k) (char-code c)))
                 (u32 (+ p 8) offset))))
      (u16 4 3)
      (table 0 "cmap" 64) (table 1 "EBLC" 96) (table 2 "EBDT" 150)
      (u16 66 1) (u16 68 3) (u16 70 1) (u32 72 12) (u16 76 4)
      (setf (aref v 120) 14 (aref v 122) 8 (aref v 149) 16)
      (dotimes (i 10) (setf (aref v (+ 150 i)) (+ 200 i)))
      v)))

(defun octets (&rest values) (coerce values '(vector (unsigned-byte 8))))

(defvar *root*
  (merge-pathnames (format nil "lispterm-loaded-font-~d-~d/"
                           (sb-posix:getpid) (get-universal-time))
                   (uiop:temporary-directory)))
(assert (nth-value 1 (ensure-directories-exist *root*)))

(defun dir (name)
  (let ((d (merge-pathnames (format nil "~a/" name) *root*)))
    (ensure-directories-exist d)
    d))

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in)
      v)))

(defun write-octets (path v)
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :error)
    (write-sequence v out))
  path)

(defun read-record (path)
  (with-open-file (in path :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (values (read in) (read in nil :eof))))))

(defun evidence (value)
  (if value
      (sb-posix:setenv "LISPTERM_FONT_EVIDENCE" value 1)
      (sb-posix:unsetenv "LISPTERM_FONT_EVIDENCE")))

(defun native (p) (uiop:native-namestring p))
(defun sequence-now () lispterm.display::*loaded-font-sequence*)
(defun record (font title) (lispterm.display::record-loaded-font font title))
(defun stem (n) (format nil "load-~d" n))
(defun member-file (d name type) (probe-file (make-pathname :name name :type type :defaults d)))
(defun entries (d) (directory (merge-pathnames "*.*" d)))

(defvar *font-file* (write-octets (merge-pathnames "fixture.otb" *root*) (fixture-octets)))
(defvar *font* (lispterm.display::read-otb *font-file*))
(defvar *title* (format nil "Lispterm-vttest-\"q\\~a-42" (code-char #x3bb)))
(defvar *keys* '(:kind :version :sequence :title :configured :path :truename
                 :length :width :height :ascent))

;;; Disabled recording attempts nothing and consumes no number.
(let ((before (sequence-now)))
  (evidence nil)
  (check :unset-returns-nil (null (record *font* "t")))
  (evidence "")
  (check :empty-returns-nil (null (record *font* "t")))
  (check :disabled-keeps-sequence (= before (sequence-now))))

(check :path-is-opened-namestring
       (equal (lispterm.display::bitmap-font-path *font*) (namestring *font-file*)))
(check :direct-font-has-no-path
       (null (lispterm.display::bitmap-font-path
              (lispterm.display::make-bitmap-font :width 8 :height 16))))

(let* ((d (dir "exact")) (before (sequence-now)))
  (evidence (native d))
  (let ((n (let ((*print-base* 16) (*print-radix* t) (*print-case* :downcase)
                 (*print-readably* t) (*print-length* 1) (*print-level* 1)
                 (*print-pretty* t) (*package* (find-package :keyword)))
             (record *font* *title*))))
    (check :sequence-increments (eql n (1+ before)))
    (check :octets-exact
           (equalp (file-octets (member-file d (stem n) "octets")) (fixture-octets)))
    (multiple-value-bind (r tail) (read-record (member-file d (stem n) "sexp"))
      (check :single-form (eq tail :eof))
      (check :closed-keys (equal (loop for k in r by #'cddr collect k) *keys*))
      (check :record-values
             (and (eq (getf r :kind) :lispterm-loaded-font) (eql (getf r :version) 1)
                  (eql (getf r :sequence) n) (equal (getf r :title) *title*)
                  (equal (getf r :configured) (uiop:getenv "LISPTERM_FONT"))
                  (equal (getf r :path) (namestring *font-file*))
                  (equal (getf r :truename) (namestring (truename *font-file*)))
                  (eql (getf r :length) 160) (eql (getf r :width) 8)
                  (eql (getf r :height) 16) (eql (getf r :ascent) 14))))
    (check :only-two-files (= 2 (length (entries d))))))

;;; Octets come from the font object, not from rereading its path.
(let* ((d (dir "memory")) (bytes (octets 1 2 3 255))
       (font (lispterm.display::make-bitmap-font
              :bytes bytes :width 8 :height 16 :ascent 14 :path (namestring *font-file*))))
  (evidence (native d))
  (let ((n (record font "m")))
    (check :memory-octets-not-path
           (and n (equalp (file-octets (member-file d (stem n) "octets")) bytes)))))

;;; Slashless directory name.
(let ((d (dir "twice")))
  (evidence (string-right-trim "/" (native d)))
  (let* ((a (record *font* "a")) (b (record *font* "b")))
    (check :second-is-next (and a b (= b (1+ a))))
    (check :both-records-kept
           (and (member-file d (stem a) "sexp") (member-file d (stem b) "sexp")))))

(let* ((d (dir "collision")) (n (1+ (sequence-now))) (marker (octets 7 7 7))
       (taken (write-octets (make-pathname :name (stem n) :type "octets" :defaults d) marker)))
  (evidence (native d))
  (check :collision-returns-nil (null (record *font* "c")))
  (check :collision-preserved (equalp (file-octets taken) marker))
  (check :collision-no-record (null (member-file d (stem n) "sexp")))
  (check :collision-consumes-number (= n (sequence-now)))
  (check :after-collision-next (eql (record *font* "c2") (1+ n))))

(let* ((d (dir "sexp-collision")) (n (1+ (sequence-now))) (marker (octets 40 41))
       (taken (write-octets (make-pathname :name (stem n) :type "sexp" :defaults d) marker)))
  (evidence (native d))
  (check :sexp-collision-returns-nil (null (record *font* "s")))
  (check :sexp-collision-preserved (equalp (file-octets taken) marker)))

(let ((missing (merge-pathnames "absent/" *root*)) (before (sequence-now)))
  (evidence (native missing))
  (check :missing-returns-nil (null (record *font* "x")))
  (check :missing-not-created (null (probe-file missing)))
  (check :missing-consumes-number (= (sequence-now) (1+ before))))

(let* ((marker (octets 9 8 7))
       (plain (write-octets (merge-pathnames "plain.bin" *root*) marker)))
  (evidence (native plain))
  (check :file-destination-returns-nil (null (record *font* "x")))
  (check :file-destination-unchanged (equalp (file-octets plain) marker)))

(let ((d (dir "threads")))
  (evidence (native d))
  (let ((ns (mapcar #'sb-thread:join-thread
                    (loop repeat 4
                          collect (sb-thread:make-thread (lambda () (record *font* "t")))))))
    (check :threads-distinct
           (and (every #'integerp ns) (= 4 (length (remove-duplicates ns)))))))

(let ((before (sequence-now)))
  (load (merge-pathnames "font.lisp" *source*))
  (check :reload-keeps-sequence (= before (sequence-now))))

(evidence nil)
(format t "~&loaded-font controls: ~d checks, ~d failures ~s~%"
        *checks* (length *failures*) (reverse *failures*))
(if *failures*
    (format t "~&Scratch kept at ~a~%" (native *root*))
    (uiop:delete-directory-tree *root* :validate t))
(uiop:quit (if *failures* 1 0))
