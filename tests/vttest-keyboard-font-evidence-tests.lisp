;;;; VERIFY-FONT-EVIDENCE controls. Exit 0 pass, 1 fail, 2 dependency, 3 verifier, 4 pin.
(require :asdf)
(require :sb-posix)
(defpackage #:vkfe-tests (:use #:cl))
(in-package #:vkfe-tests)

(defvar *here* *load-truename*)
(defun die (code what &optional c)
  (format t "~a ~a~%" what c) (finish-output) (sb-ext:exit :code code :abort t))
(handler-case (load (merge-pathnames "vttest-keyboard-sha256.lisp" *here*)) (error (c) (die 2 :dependency c)))
(handler-case (load (merge-pathnames "vttest-keyboard-font-evidence.lisp" *here*))
  (error (c) (die 3 :verifier-load c)))

(defparameter +sha+ "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(defparameter +len+ 38848)
(defun native (p) (sb-ext:native-namestring p))
(defparameter *alias*
  (native (merge-pathnames "../.build/deps/share/fonts/terminus/ter-u16n.otb" *here*)))
(defparameter *pin* (ignore-errors (native (truename *alias*))))
(defparameter *font*
  (ignore-errors (with-open-file (s *pin* :element-type '(unsigned-byte 8))
                   (let ((o (make-array (file-length s) :element-type '(unsigned-byte 8))))
                     (read-sequence o s) o))))
(unless (and *font* (= (length *font*) +len+) (string/= *alias* *pin*)
             (string= +sha+ (ignore-errors (vttest-keyboard-sha256:sha256-file *pin*))))
  (die 4 :pin *alias*))

(defun u8 (s) (sb-ext:string-to-octets s :external-format :utf-8))
(defun put (path octets) ; O_EXCL: exclusive, never follows a symlink
  (with-open-stream (s (sb-sys:make-fd-stream
                        (sb-posix:open path (logior sb-posix:o-wronly sb-posix:o-creat
                                                    sb-posix:o-excl) #o600)
                        :output t :element-type '(unsigned-byte 8)))
    (write-sequence (coerce octets '(vector (unsigned-byte 8))) s)))
(defun at (d f) (format nil "~a/~a" d f))
(defun m (n &rest edits)
  (let ((m (list :kind :lispterm-loaded-font :version 1 :sequence n :title "T"
                 :configured *pin* :path *pin* :truename *pin* :length +len+
                 :width 8 :height 16 :ascent 12)))
    (loop for (k v) on edits by #'cddr do (setf (getf m k) v)) m))
(defun txt (form) (with-standard-io-syntax (let ((*print-readably* nil)) (prin1-to-string form))))
(defun raw (s) ; :ascent spelled S
  (let ((x (txt (m 1)))) (format nil "~a~a)" (subseq x 0 (- (length x) 3)) s)))
(defun rec (d n &key (octets *font*) (form (m n)) (text (txt form)))
  (put (at d (format nil "load-~d.octets" n)) octets)
  (put (at d (format nil "load-~d.sexp" n)) (if (stringp text) (u8 text) text)))
(defun chars (n c) (make-string n :initial-element (code-char c)))

;;; EXPECT: (:pass N), or patterns each within one returned error.
(defvar *controls* '())
(defmacro defc (name expect (&key (title "T") pin sub after) &body body)
  `(push (list ,name ,expect (lambda (d) d ,@body) ,title ,pin ,sub ,after) *controls*))

(defc "load-1" '(:pass 1) () (rec d 1))
(defc "load-1,2" '(:pass 2) () (rec d 1) (rec d 2))
(defc "absent" '((:unreadable)) (:sub "absent"))
(defc "none" '((:no-loaded-font-evidence)) ())
(defc "gap" '((:incomplete 2)) () (rec d 1) (rec d 3))
(defc "half" '((:incomplete 2)) () (rec d 1) (put (at d "load-2.octets") *font*))
(dolist (f '(".x" "load-01.sexp" "load-1.txt" "notes"))
  (let ((f f)) (defc f `((:unexpected-entry ,f)) () (rec d 1) (put (at d f) #()))))
(defc "symlink" '((:not-regular-file "load-1.octets")) ()
  (put (at d "load-1.sexp") (u8 (txt (m 1)))) (sb-posix:symlink *pin* (at d "load-1.octets")))
(defc "subdir" '((:not-regular-file "load-2.sexp")) () (rec d 1) (put (at d "load-2.octets") *font*) (sb-posix:mkdir (at d "load-2.sexp") #o700))
(defc "dirlink" '((:not-directory)) (:sub "link")
  (sb-posix:mkdir (at d "real") #o700) (rec (at d "real") 1) (sb-posix:symlink "real" (at d "link")))
(defc "1026" '((:too-many)) () (rec d 1) (dotimes (i 1024) (put (at d (format nil "z~d" i)) #())))
(defc "flip" '((:sha256 1) (:octets-differ 1)) () (let ((o (copy-seq *font*))) (setf (aref o 0) (logxor 1 (aref o 0))) (rec d 1 :octets o)))
(defc "truncated" '((:field 1 :length) (:sha256 1)) () (rec d 1 :octets (subseq *font* 1)))
(defc "oversize" '((1 :too-large)) () (rec d 1 :octets (concatenate 'vector *font* #(0))))
(defc ">4096" '((1 :too-large)) () (rec d 1 :text (format nil "~a~a" (txt (m 1)) (chars 4096 32))))
(dolist (e `((:title "U") (:kind :x) (:version 2) (:sequence 2) (:width 9) (:height 17)
             (:ascent 17) (:ascent -1) (:ascent "5") (:length "38848") (:configured nil)
             (:configured "/x") (:truename "/x") (:path 5)
             (:path ,*alias* :configured ,*alias*))) ; alias MUST refuse
  (let ((e e)) (defc (txt e) `((:field 1 ,(car e))) () (rec d 1 :form (apply #'m 1 e)))))
(loop for (f x) on `(,(append (m 1) '(:width 8)) :marker-keys ,(cddr (m 1)) :marker-keys
                     ,(append (m 1) '(:x 1)) :marker-keys ,(append (m 1) '(:width)) :not-plist
                     ,(append (m 1) 1) :not-plist) by #'cddr
      do (let ((f f)) (defc (txt f) `((1 ,x)) () (rec d 1 :form f))))
(defc "trailing" '((1 :trailing-form)) () (rec d 1 :text (format nil "~a 1" (txt (m 1)))))
(defc "differ" '((:records-differ 2)) () (rec d 1) (rec d 2 :form (m 2 :ascent 11)))
(dolist (p (list *alias* "/nonexistent"))
  (let ((p p)) (defc p '((:not-truename)) (:pin p) (rec d 1))))
(defc "wrong pin" '((:pinned-sha256)) (:pin (lambda (d) (at d "p"))) (rec d 1) (put (at d "p") #(1)))
(defc "title type" '((:title-not-string)) (:title 5) (rec d 1))
(defc "read-eval" '() (:after (lambda (d) (not (probe-file (at d "sentinel")))))
  (rec d 1 :text (format nil "(:kind #.(with-open-file (s ~s :direction :output)) :version 1)"
                         (at d "sentinel"))))
(loop for (s x) on '("#1=12" () "#+x 12" () "#'f" () "cl::new-sym" ()
                     "nopkg::x" ((1 :unreadable)) "1d999999" ()) by #'cddr
      do (let ((s s)) (defc s x () (rec d 1 :text (raw s)))))
(defc "cl-user key" '((1 :marker-keys)) () (rec d 1 :text (format nil "(cl-user::kind~a" (subseq (txt (m 1)) 6))))
(defc "bad utf-8" '() () (rec d 1 :text #(40 255 41)))
(defc "deep" '((1 :not-plist)) () (rec d 1 :text (format nil "~a~a" (chars 2000 40) (chars 2000 41))))
(defc "empty" '((1 :unreadable)) () (rec d 1 :text ""))

(defun judge (v expect)
  (let ((errors (getf v :errors)))
    (if (eq (car expect) :pass)
        (equal v (list :kind :vttest-keyboard-font-evidence :version 1 :records (cadr expect)
                       :title "T" :sha256 +sha+ :length +len+ :truename *pin*
                       :claim :loaded-octets-equal-pinned))
        (and (equal (loop for k in v by #'cddr collect k)
                    '(:kind :version :records :title :claim :errors))
             (null (getf v :claim)) errors
             (every (lambda (w) (some (lambda (e) (subsetp w e :test #'equal)) errors))
                    expect)))))

(let* ((made (sb-posix:mkdtemp (format nil "~a/vkfe-XXXXXX" (string-right-trim
                                   "/" (or (sb-posix:getenv "TMPDIR") "/tmp")))))
       (root (string-right-trim "/" (native (truename (at made "")))))
       (pass 0) (failures '()))
  (format t "fixture root ~a~%" root)
  (loop for (name expect fn title pin sub after) in (reverse *controls*)
        for i from 1 for d = (at root i)
        do (handler-case
               (let ((v (progn (sb-posix:mkdir d #o700) (funcall fn d)
                               (vttest-keyboard-font-evidence:verify-font-evidence
                                (if sub (at d sub) d) title
                                (if (functionp pin) (funcall pin d) (or pin *pin*))))))
                 (if (and (judge v expect) (or (null after) (funcall after d)))
                     (incf pass)
                     (push (list i name v) failures)))
             (error (c) (push (list i name :harness (princ-to-string c)) failures))))
  (dolist (f (setf failures (reverse failures))) (format t "FAIL ~s~%" f))
  (put (at root "failures.sexp") (u8 (format nil "~s" failures)))
  (format t "controls ~d passed ~d failed ~d root ~a~%" (length *controls*) pass
          (length failures) root)
  (finish-output) (sb-ext:exit :code (if failures 1 0)))
