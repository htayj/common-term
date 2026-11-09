;;;; Lispterm font. Native Lisp display implementation.

(in-package #:lispterm.display)

(defun load-font (path size)
  (read-otb
   (or path (uiop/os:getenv "LISPTERM_FONT")
       (first
        (directory
         (format nil "/nix/store/*-terminus-font-*/share/fonts/terminus/ter-u~dn.otb" size)))
       (error "Set LISPTERM_FONT to a Terminus OpenType bitmap font"))))

(defvar *loaded-font-sequence* 0
  "Process-local number of the last loaded-font evidence record attempted.")

(defvar *loaded-font-lock* (sb-thread:make-mutex :name "lispterm loaded font"))

(defun create-loaded-font-file (path element-type)
  "Open PATH as a new file. Refuse an existing file and never create parents."
  (if (eq element-type :character)
      (open path :direction :output :if-exists :error :if-does-not-exist :create
                 :external-format :utf-8)
      (open path :direction :output :if-exists :error :if-does-not-exist :create
                 :element-type element-type)))

(defun write-loaded-font-record (path value)
  ;; Closing without :abort keeps whatever was written for the harness to refuse.
  (let ((s (create-loaded-font-file path :character)))
    (unwind-protect
         (let ((*print-readably* nil) (*print-escape* t) (*print-pretty* nil)
               (*print-circle* nil) (*print-length* nil) (*print-level* nil)
               (*print-base* 10) (*print-radix* nil) (*print-case* :upcase)
               (*print-array* t) (*print-gensym* t)
               (*package* (find-package :cl)) (*readtable* (copy-readtable nil)))
           (write value :stream s))
      (close s))))

(defun record-loaded-font (font title)
  "When LISPTERM_FONT_EVIDENCE names a directory, copy the octets FONT holds to
a new load-N.octets there, then describe them in a new load-N.sexp. Return N,
or nil when unset or on any error."
  (let ((directory (uiop:getenv "LISPTERM_FONT_EVIDENCE")))
    (when (and directory (plusp (length directory)))
      (handler-case
          (let* ((n (sb-thread:with-mutex (*loaded-font-lock*)
                      (incf *loaded-font-sequence*)))
                 (base (uiop:ensure-directory-pathname
                        (uiop:parse-native-namestring directory)))
                 (name (format nil "load-~d" n))
                 (bytes (bitmap-font-bytes font))
                 (path (bitmap-font-path font)))
            (let ((s (create-loaded-font-file
                      (make-pathname :name name :type "octets" :defaults base)
                      '(unsigned-byte 8))))
              (unwind-protect (write-sequence bytes s) (close s)))
            (write-loaded-font-record
             (make-pathname :name name :type "sexp" :defaults base)
             (list :kind :lispterm-loaded-font :version 1 :sequence n :title title
                   :configured (uiop:getenv "LISPTERM_FONT") :path path
                   :truename (and path (ignore-errors (namestring (truename path))))
                   :length (length bytes) :width (bitmap-font-width font)
                   :height (bitmap-font-height font) :ascent (bitmap-font-ascent font)))
            n)
        (error () nil)))))

(defun glyph-bitmap (font char)
  (or (gethash char (bitmap-font-cache font))
      (setf (gethash char (bitmap-font-cache font)) (otb-glyph font char))))
