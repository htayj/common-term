;;;; Synthetic scale controls over temporary fixture roots. Not wired.
(require :sb-posix)
(defvar cl-user::*vtscale-evaluated* nil)
(dolist (f '("vttest-screen-native-contract" "vttest-double-size-native-contract"
             "vttest-charsets-native-contract" "vttest-screen-frame-tests"))
  (load (merge-pathnames (concatenate 'string f ".lisp") *load-truename*)))
(defpackage :vttest-scale-contract-tests (:use :cl) (:export :run-tests))
(in-package :vttest-scale-contract-tests)

(defvar *pass* 0)
(defvar *roots* nil)
(defparameter +env+ '("LISPTERM_VTTEST_SCALE" "LISPTERM_SCREEN_SCALE" "LISPTERM_DOUBLE_SIZE_SCALE"))
(defun ok (label test) (unless test (error "Control failed: ~s" label)) (incf *pass*))
(defun refuse (label thunk) (ok label (handler-case (progn (funcall thunk) nil) (error () t))))
(defun root ()
  (let ((r (concatenate 'string (sb-posix:mkdtemp "/tmp/vtscale-XXXXXX") "/")))
    (push r *roots*)
    (dolist (d '("native/" "recorder/")) (ensure-directories-exist (concatenate 'string r d)))
    r))
(defun put (root path text)
  (with-open-file (s (concatenate 'string root path) :direction :output :if-exists :error)
    (write-string text s))
  root)
(defun with-env (name value thunk)
  (let ((old (sb-posix:getenv name)))
    (unwind-protect
         (progn (if value (sb-posix:setenv name value 1) (sb-posix:unsetenv name)) (funcall thunk))
      (if old (sb-posix:setenv name old 1) (sb-posix:unsetenv name)))))
(defun with-envs (names thunk)
  (if names (with-env (first names) nil (lambda () (with-envs (rest names) thunk))) (funcall thunk)))

(defun manifest (area backend &rest overrides)
  "All eleven keys; :DROP removes a key."
  (let ((m (list :kind :vttest-area-manifest :version 1 :area area :backend backend :scale 1
                 :scope-status :frozen :conditions nil :scenarios nil :required-count 1
                 :native-driver (format nil "charsets-~a-v1" backend) :pins nil)))
    (loop for (k v) on overrides by #'cddr
          do (if (eq v :drop) (remf m k) (setf (getf m k) v)))
    (prin1-to-string m)))

;;; (FN AREA DRIVER+PINS-REQUIRED DRIVER-CHECKED ENV-NAMES); extension point.
(defparameter +areas+
  '((vttest-screen-native-contract::manifest-scale "screen" t nil
     ("LISPTERM_VTTEST_SCALE" "LISPTERM_SCREEN_SCALE"))
    (vttest-double-size-native-contract::manifest-scale "double-size-characters" nil nil
     ("LISPTERM_VTTEST_SCALE" "LISPTERM_DOUBLE_SIZE_SCALE"))
    (vttest-charsets-native-contract::manifest-scale "character-sets" t t
     ("LISPTERM_VTTEST_SCALE"))))

(defun scale-of (fn text backend) (funcall fn (put (root) "MANIFEST.sexp" text) backend))

(defun manifest-controls ()
  (loop for (fn area full driver names) in +areas+ do
    (flet ((m (backend &rest o) (apply #'manifest area backend o))
           (no (label backend text)
             (refuse (list area label) (lambda () (scale-of fn text backend)))))
      (ok (list area :x11) (eql 1 (scale-of fn (m "x11") "x11")))
      (dolist (s '(1 2))
        (ok (list area :wayland s) (eql s (scale-of fn (m "wayland" :scale s) "wayland"))))
      (no :x11-2 "x11" (m "x11" :scale 2))
      (no :foreign-2 "foo" (m "foo" :scale 2))
      (dolist (s '("1" 1.0 0 3 nil)) (no (list :scale s) "wayland" (m "wayland" :scale s)))
      (no :missing "wayland" (m "wayland" :scale :drop))
      (no :duplicate "wayland" (concatenate 'string "(:SCALE 1 " (subseq (m "wayland") 1)))
      (no :backend "wayland" (m "x11"))
      (no :area "x11" (m "x11" :area "keyboard"))
      (no :version "x11" (m "x11" :version 2))
      (no :frozen "x11" (m "x11" :scope-status :draft))
      (no :unknown "x11" (m "x11" :extra 1))
      (no :trailing "x11" (concatenate 'string (m "x11") " 1"))
      (no :read-eval "x11" "#.(setf cl-user::*vtscale-evaluated* t)")
      (ok (list area :not-evaluated) (null cl-user::*vtscale-evaluated*))
      (if full
          (no :no-pins "x11" (m "x11" :pins :drop))
          (ok (list area :optional) (eql 1 (scale-of fn (m "x11" :pins :drop :native-driver :drop) "x11"))))
      (when driver (no :driver "x11" (m "x11" :native-driver "charsets-wayland-v1")))
      (dolist (name names)
        (with-env name "1" (lambda () (no (list :env name) "wayland" (m "wayland" :scale 2))))
        (with-env name "2"
          (lambda () (ok (list area name) (eql 2 (scale-of fn (m "wayland" :scale 2) "wayland")))))
        (ok (list area :restored name) (null (sb-posix:getenv name)))))))

(defun selected-controls ()
  (let ((sel 'vttest-charsets-native-contract::selected-scale)
        (r (put (root) "MANIFEST.sexp" (manifest "character-sets" "wayland" :scale 2))))
    (ok :sel-manifest (eql 2 (funcall sel r "wayland" nil)))
    (ok :sel-explicit (eql 2 (funcall sel r "wayland" 2)))
    (refuse :sel-mismatch (lambda () (funcall sel r "wayland" 1)))
    (ok :sel-rootless (eql 1 (funcall sel nil "x11" nil)))
    (ok :sel-rootless-2 (eql 2 (funcall sel nil "wayland" 2)))
    (refuse :sel-rootless-x11-2 (lambda () (funcall sel nil "x11" 2)))))

(defun ledger (fn rows)
  (let ((r (root)))
    (put r "recorder/pause-01-request" "r")
    (put r "recorder/pause-01-capture.exit" "0")
    (put r "native/stage-events.sexp" (format nil "~{~s~%~}" rows))
    (funcall fn r "x11")))

(defparameter +base+ '((1 :request) (1 :invoked) (1 :completed 0)))

(defun ledger-controls ()
  (loop for (fn kind) in '((vttest-screen-native-contract::build-ledger :vttest-screen-native-stages)
                           (vttest-double-size-native-contract::build-ledger
                            :vttest-double-size-native-stages)) do
    (let* ((l (ledger fn (append +base+ '((1 :physical-size-refused 1)))))
           (e (first (getf l :entries))))
      (ok (list kind :schema) (equal (loop for (k) on l by #'cddr collect k)
                                     '(:kind :version :backend :entries)))
      (ok (list kind :kind) (and (eq kind (getf l :kind)) (= 1 (length (getf l :entries)))))
      (ok (list kind :entry) (and (eq t (getf e :invoked)) (eq t (getf e :completed))
                                  (eql 0 (getf e :capture-exit)) (null (getf e :receipt))
                                  (equal "recorder/pause-01-capture.exit"
                                         (getf (getf e :exit-origin) :path))))
      (ok (list kind :annotation-only) (equal l (ledger fn +base+)))
      (dolist (rows (list* '((1 :request) (1 :invoked) (1 :completed 3) (1 :physical-size-refused 1))
                           '((1 :request) (1 :invoked) (1 :physical-size-refused 1))
                           (append +base+ '((1 :physical-size-refused 1) (1 :physical-size-refused 1)))
                           (mapcar (lambda (x) (append +base+ (list x)))
                                   '((2 :physical-size-refused 1) (1 :physical-size-refused)
                                     (1 :physical-size-refused "1") (1 :physical-size-refused -1)
                                     (1 :physical-size-refused 256)))))
        (refuse (list kind rows) (lambda () (ledger fn rows)))))))

(defun frame-controls ()
  "Logical frame linkage only; no physical, pin or native proof."
  (flet ((args (scale &rest keys)
           (let ((a (vttest-screen-frame-tests::fixture :backend :wayland)))
             (setf (getf (nth 4 a) :scale) scale)
             (append a keys)))
         (yes (a) (apply #'vttest-screen-frame:validate-frame a))
         (refused (a) (handler-case (progn (apply #'vttest-screen-frame:validate-frame a) nil)
                        (vttest-screen-frame:frame-refusal () t))))
    (ok :frame-default (yes (args 1)))
    (ok :frame-1 (yes (args 1 :scale 1)))
    (ok :frame-2 (yes (args 2 :scale 2)))
    (ok :receipt-2-default (refused (args 2)))
    (ok :receipt-1-selected-2 (refused (args 1 :scale 2)))
    (ok :selected-3 (refused (args 3 :scale 3)))))

(defun run-tests ()
  (setf *pass* 0 *roots* nil)
  (unwind-protect
       (with-envs +env+ (lambda () (manifest-controls) (selected-controls)
                          (ledger-controls) (frame-controls)))
    (dolist (r *roots*) (sb-ext:delete-directory r :recursive t)))
  (format t "scale contract controls: ~d passed~%" *pass*)
  *pass*)
