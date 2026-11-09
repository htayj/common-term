;;;; Read-only verifier of RECORD-LOADED-FONT evidence (src/display/font.lisp).

(require :asdf)
(require :sb-posix)
(load (merge-pathnames "vttest-keyboard-sha256.lisp" *load-truename*))

(defpackage #:vttest-keyboard-font-evidence
  (:use #:cl)
  (:export #:verify-font-evidence))

(defpackage #:vttest-font-marker (:use) (:import-from #:cl #:nil))

(in-package #:vttest-keyboard-font-evidence)

(defparameter *pinned-sha256*
  "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(defparameter *pinned-length* 38848)
(defparameter *keys* '(:kind :version :sequence :title :configured :path
                       :truename :length :width :height :ascent))

(defvar *errors*)
(define-condition refusal (error) ((reason :initarg :reason :reader reason)))
(defun refuse (&rest reason) (error 'refusal :reason reason))
(defun note (&rest reason) (push reason *errors*))

(defun native (name) (if (stringp name) name (sb-ext:native-namestring name)))

(defun kind (stat)
  (let ((mode (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt)))
    (cond ((= mode sb-posix:s-ifreg) :file)
          ((= mode sb-posix:s-ifdir) :directory))))

(defun read-regular (path limit)
  "Octets of the regular non-symlink file PATH, at most LIMIT."
  (let ((fd (sb-posix:open path (logior sb-posix:o-rdonly sb-posix:o-nofollow
                                        sb-posix:o-nonblock)))
        (stream nil))
    (unwind-protect
         (let* ((stat (sb-posix:fstat fd)) (size (sb-posix:stat-size stat)))
           (unless (eq (kind stat) :file) (refuse :not-regular-file path))
           (when (> size limit) (refuse :too-large path size))
           (setf stream (sb-sys:make-fd-stream fd :input t
                                                  :element-type '(unsigned-byte 8)))
           (let ((octets (make-array size :element-type '(unsigned-byte 8))))
             (unless (and (= size (read-sequence octets stream))
                          (null (read-byte stream nil)))
               (refuse :size-changed path))
             octets))
      (if stream (close stream) (sb-posix:close fd)))))

(defun entries (directory)
  "All readdir names, hidden too, but . and .."
  (let ((handle (sb-posix:opendir directory)) (names '()))
    (unwind-protect
         (loop for entry = (sb-posix:readdir handle)
               until (sb-alien:null-alien entry)
               do (let ((name (sb-posix:dirent-name entry)))
                    (unless (member name '("." "..") :test #'string=)
                      (when (> (length names) 1024) (refuse :too-many))
                      (push name names))))
      (sb-posix:closedir handle))
    names))

(defun parse-entry (name)
  (let ((dot (position #\. name)))
    (when (and dot (< 5 dot 12) (string= "load-" name :end2 5)
               (char/= #\0 (char name 5))
               (every (lambda (c) (char<= #\0 c #\9)) (subseq name 5 dot))
               (member (subseq name (1+ dot)) '("octets" "sexp") :test #'string=))
      (values (parse-integer name :start 5 :end dot) (subseq name (1+ dot))))))

(defparameter *marker-readtable* ; no # dispatch
  (let ((readtable (copy-readtable nil)))
    (set-syntax-from-char #\# #\a readtable)
    readtable))

(defun read-marker (octets)
  "The only form in OCTETS: a plist with unique keys exactly *KEYS*."
  (let ((form (with-input-from-string
                  (s (sb-ext:octets-to-string octets :external-format :utf-8))
                (let ((*readtable* *marker-readtable*) (*read-eval* nil)
                      (*read-base* 10) (*read-suppress* nil)
                      (*package* (find-package '#:vttest-font-marker)))
                  (prog1 (read s)
                    (unless (eq s (read s nil s)) (refuse :trailing-form)))))))
    (unless (and (listp form) (null (cdr (last form))) (evenp (length form)))
      (refuse :not-plist))
    (let ((keys (loop for key in form by #'cddr collect key)))
      (unless (and (= (length keys) (length (remove-duplicates keys))
                      (length *keys*))
                   (null (set-difference keys *keys*)))
        (refuse :marker-keys keys)))
    form))

(defun resolves-to (path truename)
  (equal truename (ignore-errors (native (truename (sb-ext:parse-native-namestring path))))))

(defun check-record (n directory title truename pinned)
  (let* ((base (format nil "~a/load-~d." directory n))
         (octets (read-regular (concatenate 'string base "octets") *pinned-length*))
         (marker (read-marker (read-regular (concatenate 'string base "sexp") 4096)))
         (before (length *errors*)))
    (flet ((v (key) (getf marker key))
           (want (key ok) (unless ok (note :field n key (getf marker key)))))
      (want :kind (eq (v :kind) :lispterm-loaded-font))
      (want :version (eql (v :version) 1))
      (want :sequence (eql (v :sequence) n))
      (want :title (equal (v :title) title))
      (want :path (and (stringp (v :path)) (equal (v :path) truename)))
      (want :configured (equal (v :configured) (v :path)))
      (want :truename (equal (v :truename) truename))
      (want :length (eql (v :length) (length octets)))
      (want :width (eql (v :width) 8))
      (want :height (eql (v :height) 16))
      (want :ascent (typep (v :ascent) '(integer 0 16))))
    (unless (equalp octets pinned) ; PINNED has the pinned digest
      (note :sha256 n) (note :octets-differ n))
    (when (= before (length *errors*))
      (let ((copy (copy-list marker))) (remf copy :sequence) copy))))

(defun check-evidence (directory title truename)
  (unless (stringp title) (refuse :title-not-string))
  (unless (resolves-to truename truename) (refuse :not-truename truename))
  (let ((pinned (read-regular truename *pinned-length*))
        (table (make-hash-table))
        (reference nil))
    (unless (and (string= (vttest-keyboard-sha256:sha256-file truename) *pinned-sha256*)
                 (equalp pinned (read-regular truename *pinned-length*)))
      (refuse :pinned-sha256))
    (unless (eq (kind (sb-posix:lstat directory)) :directory)
      (refuse :not-directory directory))
    (dolist (name (entries directory))
      (multiple-value-bind (n type) (parse-entry name)
        (cond ((null n) (note :unexpected-entry name))
              ((not (eq :file (kind (sb-posix:lstat (format nil "~a/~a" directory name)))))
               (note :not-regular-file name))
              (t (push type (gethash n table))))))
    (when (zerop (hash-table-count table)) (refuse :no-loaded-font-evidence))
    (let ((count (loop for n being the hash-keys of table maximize n)))
      (loop for n from 1 to count
            for types = (gethash n table)
            do (if (/= 2 (length types))
                   (note :incomplete n types)
                   (let ((marker (handler-case
                                     (check-record n directory title truename pinned)
                                   (refusal (c) (apply #'note n (reason c)) nil)
                                   (error (c) (note n :unreadable (type-of c)) nil))))
                     (cond ((null marker))
                           ((null reference) (setf reference marker))
                           ((not (equal marker reference)) (note :records-differ n))))))
      count)))

(defun verify-font-evidence (dir expected-title pinned-truename)
  "Verdict plist and errors; claims only for defect-free load-1..N."
  (let ((*errors* '()) (count 0) (truename nil))
    (handler-case
        (setf truename (native pinned-truename)
              count (check-evidence (string-right-trim "/" (native dir))
                                    expected-title truename))
      (refusal (c) (apply #'note (reason c)))
      (error (c) (note :unreadable (type-of c))))
    (let ((errors (reverse *errors*)))
      (values (list* :kind :vttest-keyboard-font-evidence :version 1 :records count
                     :title expected-title
                     (if errors
                         (list :claim nil :errors errors)
                         (list :sha256 *pinned-sha256* :length *pinned-length*
                               :truename truename :claim :loaded-octets-equal-pinned)))
              errors))))
