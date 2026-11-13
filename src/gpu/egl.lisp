(in-package #:lispterm.gpu)

;;; Libraries are resolved only when OPEN-CONTEXT is requested. Every call into
;;; a driver is non-interruptible and masks all SBCL floating-point traps.
(defmacro with-driver-call (&body body)
  `(sb-sys:without-interrupts
     (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow
                                   :underflow :inexact :denormalized-operand)
       ,@body)))

(defvar *native-libraries* nil)
(defvar *native-lock* (sb-thread:make-mutex :name "GPU native loader"))

(defun native-library-path (name)
  (let* ((root (uiop:getenv "LISPTERM_GPU_RUNTIME"))
         (path (and root (merge-pathnames name (uiop:ensure-directory-pathname root)))))
    (unless (and path (probe-file path))
      (unavailable 'open-context
                   (format nil "Pinned GPU library missing: ~A; use scripts/sbcl with the GPU runtime" name)))
    (namestring (truename path))))

(defun validate-driver-paths ()
  (let* ((root (uiop:getenv "LISPTERM_GPU_RUNTIME"))
         (directory (and root (uiop:ensure-directory-pathname root))))
    (unless directory (unavailable 'open-context "Pinned GPU runtime is not selected"))
    (dolist (setting '(("__EGL_VENDOR_LIBRARY_FILENAMES" . "share/glvnd/egl_vendor.d/50_mesa.json")
                       ("LIBGL_DRIVERS_PATH" . "lib/dri/")
                       ("GBM_BACKENDS_PATH" . "lib/gbm/")))
      (let ((actual (uiop:getenv (car setting)))
            (expected (merge-pathnames (cdr setting) directory)))
        (unless (and actual (probe-file actual) (probe-file expected)
                     (equal (truename actual) (truename expected)))
          (unavailable 'open-context (format nil "Launcher must pin ~A to runtime closure" (car setting)))))))
  t)

(defun native-driver-mappings ()
  "Reject a preloaded or loader-selected Mesa from any other closure."
  (let* ((root (uiop:ensure-directory-pathname (uiop:getenv "LISPTERM_GPU_RUNTIME")))
         (allowed (append
                   (mapcar #'native-library-path '("lib/libEGL_mesa.so.0" "lib/gbm/dri_gbm.so"))
                   (loop for path in (uiop:directory-files (merge-pathnames "lib/" root))
                         when (uiop:string-prefix-p "libgallium-" (file-namestring path))
                           collect (namestring (truename path)))))
         (mapped nil))
    (with-open-file (stream #p"/proc/self/maps")
      (loop for line = (read-line stream nil) while line
            for start = (position #\/ line)
            when start do
              (let* ((path (subseq line start)) (name (file-namestring path)))
                (when (or (uiop:string-prefix-p "libEGL_mesa" name)
                          (uiop:string-prefix-p "libgallium-" name)
                          (string= name "dri_gbm.so"))
                  (unless (member path allowed :test #'equal)
                    (unavailable 'open-context
                                 (format nil "Mismatched Mesa driver already mapped: ~A" path)))
                  (pushnew path mapped :test #'equal)))))
    (sort mapped #'string<)))

(defun ensure-native-libraries ()
  (sb-thread:with-mutex (*native-lock*)
    (validate-driver-paths)
    (unless *native-libraries*
      ;; GLVND must see exactly the matching vendor, not the host driver JSON.
      (let ((json (native-library-path "share/glvnd/egl_vendor.d/50_mesa.json")))
        (unless (equal (uiop:getenv "__EGL_VENDOR_LIBRARY_FILENAMES") json)
          (unavailable 'open-context "Launcher must select the pinned EGL vendor JSON")))
      (let ((loaded nil))
        (unwind-protect
             (progn
               (dolist (entry '((:egl . "lib/libEGL.so.1")
                                (:gles . "lib/libGLESv2.so.2")
                                (:gbm . "lib/libgbm.so.1")))
                 (push (cons (car entry)
                             (with-driver-call
                               (cffi:load-foreign-library (native-library-path (cdr entry)))))
                       loaded))
               (setf *native-libraries* (reverse loaded) loaded nil))
          (dolist (entry loaded)
            (with-driver-call (cffi:close-foreign-library (cdr entry))))))))
  t)

(defmacro define-driver-function (name library c-name result &rest arguments)
  `(progn
     (defun ,name ,(mapcar #'first arguments)
       (with-driver-call
         (cffi:foreign-funcall-pointer
          (cffi:foreign-symbol-pointer ,c-name
                                      :library (cdr (assoc ,library *native-libraries*)))
          () ,@(loop for (var type) in arguments append (list type var)) ,result)))
     (export ',name)))

(define-driver-function egl-get-error :egl "eglGetError" :int)
(define-driver-function egl-get-platform-display :egl "eglGetPlatformDisplay" :pointer
  (platform :unsigned-int) (native :pointer) (attributes :pointer))
(define-driver-function egl-initialize :egl "eglInitialize" :unsigned-int
  (display :pointer) (major :pointer) (minor :pointer))
(define-driver-function egl-query-string :egl "eglQueryString" :string
  (display :pointer) (name :int))
(define-driver-function egl-bind-api :egl "eglBindAPI" :unsigned-int (api :unsigned-int))
(define-driver-function egl-choose-config :egl "eglChooseConfig" :unsigned-int
  (display :pointer) (attributes :pointer) (configs :pointer) (size :int) (count :pointer))
(define-driver-function egl-create-context :egl "eglCreateContext" :pointer
  (display :pointer) (config :pointer) (share :pointer) (attributes :pointer))
(define-driver-function egl-make-current :egl "eglMakeCurrent" :unsigned-int
  (display :pointer) (draw :pointer) (read :pointer) (context :pointer))
(define-driver-function egl-destroy-context :egl "eglDestroyContext" :unsigned-int
  (display :pointer) (context :pointer))
(define-driver-function egl-terminate :egl "eglTerminate" :unsigned-int (display :pointer))
(define-driver-function egl-get-proc-address :egl "eglGetProcAddress" :pointer (name :string))

(defun egl-check (result operation)
  (when (or (and (integerp result) (zerop result))
            (and (cffi:pointerp result) (cffi:null-pointer-p result)))
    (unavailable operation (format nil "EGL error #x~X" (egl-get-error))))
  result)

(defun extension-present-p (name extensions)
  (member name (uiop:split-string (or extensions "") :separator " ") :test #'string=))

(export '(with-driver-call ensure-native-libraries extension-present-p))
