(in-package #:lispterm.gpu)

(define-condition gpu-ownership-error (gpu-unavailable) ())
(define-condition gpu-software-rejected (gpu-unavailable) ())
(define-condition gpu-cleanup-error (gpu-unavailable)
  ((context :initarg :context :reader cleanup-error-context)))

(defclass graphics-context ()
  ((thread :initform sb-thread:*current-thread* :reader context-thread)
   (pid :initform (sb-unix:unix-getpid) :reader context-pid)
   (display :initform (cffi:null-pointer) :accessor context-display)
   (native :initform (cffi:null-pointer) :accessor context-native)
   (gbm :initform (cffi:null-pointer) :accessor context-gbm)
   (fd :initform nil :accessor context-fd)
   (initialized :initform nil :accessor context-initialized-p)
   (closed :initform nil :accessor context-closed-p)
   (info :initform nil :accessor context-metadata)))

(defvar *live-contexts* nil)
(defvar *context-lock* (sb-thread:make-mutex :name "GPU context registry"))

(defun require-no-live-contexts ()
  (when *live-contexts*
    (unavailable 'save-image "Close every GPU renderer/context before saving an image")))
(pushnew 'require-no-live-contexts sb-ext:*save-hooks*)

(defun check-context-identity (context)
  (unless (and (= (context-pid context) (sb-unix:unix-getpid))
               (eq (context-thread context) sb-thread:*current-thread*))
    (error 'gpu-ownership-error :operation 'assert-context-owner
           :detail "GPU resources belong to their creating process and thread")))

(defgeneric assert-context-owner (context))
(defmethod assert-context-owner ((context graphics-context))
  (check-context-identity context)
  (when (context-closed-p context)
    (error 'gpu-ownership-error :operation 'assert-context-owner :detail "GPU context is closed"))
  ;; Several contexts may share an owner. Rebind explicitly, never assume the
  ;; driver's thread-local current context still equals this object.
  (egl-check (egl-make-current (context-display context) (cffi:null-pointer)
                               (cffi:null-pointer) (context-native context))
             'assert-context-owner)
  t)

(defun extension-address (name)
  (let ((address (egl-get-proc-address name)))
    (when (cffi:null-pointer-p address)
      (unavailable 'open-context (format nil "Missing EGL extension function ~A" name)))
    address))

(defun query-device-string (device name)
  (with-driver-call
    (cffi:foreign-funcall-pointer (extension-address "eglQueryDeviceStringEXT") ()
                                 :pointer device :int name :string)))

(defun display-device (display)
  (cffi:with-foreign-object (device :intptr)
    (egl-check
     (with-driver-call
       (cffi:foreign-funcall-pointer (extension-address "eglQueryDisplayAttribEXT") ()
                                    :pointer display :int #x322C :pointer device :unsigned-int))
     'query-display-device)
    (cffi:make-pointer (cffi:mem-ref device :intptr))))

(defun software-egl-device ()
  (cffi:with-foreign-objects ((count :int) (devices :pointer 64))
    (egl-check
     (with-driver-call
       (cffi:foreign-funcall-pointer (extension-address "eglQueryDevicesEXT") ()
                                    :int 64 :pointer devices :pointer count :unsigned-int))
     'query-devices)
    (or (loop for i below (min 64 (cffi:mem-ref count :int))
              for device = (cffi:mem-aref devices :pointer i)
              when (extension-present-p "EGL_MESA_device_software"
                                        (query-device-string device #x3055))
                return device)
        (unavailable 'open-context "No explicitly identified EGL software device"))))

(defun require-hardware-context (context)
  "Reject software using EGL device classification, not a renderer-name guess."
  (when (getf (context-metadata context) :software-p)
    (error 'gpu-software-rejected :operation 'open-context
           :detail "EGL_MESA_device_software is incompatible with required hardware rendering"))
  (unless (and (getf (context-metadata context) :device)
               (getf (context-metadata context) :device-associated-p))
    (unavailable 'open-context "GL context is not associated with the opened DRM render node"))
  t)

(defun collect-context-info (context requested-device)
  (let* ((display (context-display context))
         (device (display-device display))
         (extensions (query-device-string device #x3055))
         (software (not (null (extension-present-p "EGL_MESA_device_software" extensions))))
         (render-node (and (extension-present-p "EGL_EXT_device_drm_render_node" extensions)
                           (query-device-string device #x3377)))
         (associated (and requested-device render-node
                          (equal (truename requested-device) (truename render-node)))))
    (unless (>= (gl-integer #x821B) 3)
      (unavailable 'open-context "OpenGL ES 3 is required"))
    (list :vendor (gl-get-string #x1F00) :renderer (gl-get-string #x1F01)
          :version (gl-get-string #x1F02) :shading-language (gl-get-string #x8B8C)
          :egl-vendor (egl-query-string display #x3053)
          :egl-version (egl-query-string display #x3054)
          :egl-extensions (egl-query-string display #x3055)
          :device-extensions extensions :device render-node
          :device-associated-p (not (null associated)) :software-p software
          :driver (if software "EGL_MESA_device_software"
                      (and (not (cffi:null-pointer-p (context-gbm context)))
                           (gbm-device-get-backend-name (context-gbm context))))
          :runtime (uiop:getenv "LISPTERM_GPU_RUNTIME")
          :egl-library (native-library-path "lib/libEGL.so.1")
          :gles-library (native-library-path "lib/libGLESv2.so.2")
          :gbm-library (native-library-path "lib/libgbm.so.1")
          :mesa-vendor-json (native-library-path "share/glvnd/egl_vendor.d/50_mesa.json")
          :native-driver-mappings (native-driver-mappings)
          :max-texture-size (gl-integer #x0D33)
          :max-renderbuffer-size (gl-integer #x84E8)
          :max-viewport-dims (gl-integers #x0D3A 2)
          :max-vertex-attribs (gl-integer #x8869)
          :max-color-attachments (gl-integer #x8CDF)
          :gl-major (gl-integer #x821B) :gl-minor (gl-integer #x821C)
          :max-framebuffer-width (if (or (> (gl-integer #x821B) 3)
                                         (>= (gl-integer #x821C) 1))
                                     (gl-integer #x9315) (gl-integer #x84E8))
          :max-framebuffer-height (if (or (> (gl-integer #x821B) 3)
                                          (>= (gl-integer #x821C) 1))
                                      (gl-integer #x9316) (gl-integer #x84E8)))))

(defun release-context-handles (context)
  ;; Commit each successful release separately. A failed EGL release retains its
  ;; handle and registry entry so CLOSE-CONTEXT can be retried by the owner.
  (handler-case
      (progn
          ;; Construction failures keep strong GL ownership. Drain only while this
          ;; exact context is current, before any EGL unbind or handle release.
          (when (and (not (cffi:null-pointer-p (context-native context)))
                     (pending-gl-objects-p (context-native context)))
            (assert-context-owner context)
            (drain-gl-object-cleanup))
        (unless (cffi:null-pointer-p (context-display context))
          (when (context-initialized-p context)
            (egl-check (egl-make-current (context-display context) (cffi:null-pointer)
                                         (cffi:null-pointer) (cffi:null-pointer))
                       'close-context-unbind)
            (unless (cffi:null-pointer-p (context-native context))
              (egl-check (egl-destroy-context (context-display context) (context-native context))
                         'close-context-destroy)
              (setf (context-native context) (cffi:null-pointer)))
            ;; Platform-device displays can be shared. EGL initialization is not
            ;; reference counted, so only the final live owner terminates one.
            (unless (some (lambda (other)
                            (and (not (eq other context))
                                 (cffi:pointer-eq (context-display other) (context-display context))))
                          *live-contexts*)
              (egl-check (egl-terminate (context-display context)) 'close-context-terminate))
            (setf (context-initialized-p context) nil))
          (setf (context-display context) (cffi:null-pointer)))
        (unless (cffi:null-pointer-p (context-gbm context))
          (gbm-device-destroy (context-gbm context))
          (setf (context-gbm context) (cffi:null-pointer)))
        (close-render-node (context-fd context))
        (setf (context-fd context) nil (context-closed-p context) t))
    (gl-object-cleanup-error (condition)
      (pushnew context *live-contexts*)
      (error condition))
    (gpu-unavailable (condition)
      ;; Failed creation also needs a reachable cleanup handle. Saving an image
      ;; continues to be prohibited until this retained context is closed.
      (pushnew context *live-contexts*)
      (error 'gpu-cleanup-error :context context :operation 'close-context
             :detail (format nil "~A; retry CLOSE-CONTEXT on CLEANUP-ERROR-CONTEXT" condition)))))

(defun create-context (software device)
  (sb-thread:with-mutex (*context-lock*)
  (let ((context (make-instance 'graphics-context)) (success nil))
    (sb-sys:without-interrupts
      (unwind-protect
           (progn
             (setf (context-display context)
                   (if software
                       (egl-get-platform-display #x313F (software-egl-device) (cffi:null-pointer))
                       (progn
                         (setf (context-fd context) (open-render-node device)
                               (context-gbm context) (egl-check (gbm-create-device (context-fd context)) 'gbm-create-device))
                         (egl-get-platform-display #x31D7 (context-gbm context) (cffi:null-pointer)))))
             (egl-check (context-display context) 'egl-get-platform-display)
             (cffi:with-foreign-objects ((major :int) (minor :int))
               (egl-check (egl-initialize (context-display context) major minor) 'egl-initialize))
             (setf (context-initialized-p context) t)
             (unless (extension-present-p "EGL_KHR_surfaceless_context"
                                           (egl-query-string (context-display context) #x3055))
               (unavailable 'open-context "EGL_KHR_surfaceless_context required"))
             (egl-check (egl-bind-api #x30A0) 'egl-bind-api)
             (cffi:with-foreign-objects ((attributes :int 7) (config :pointer) (count :int)
                                         (context-attributes :int 3))
               ;; No surface is used: do not require pbuffer support on GBM.
               (loop for value in (list #x3033 0 #x3040 #x0040 #x3024 8 #x3038)
                     for i from 0 do (setf (cffi:mem-aref attributes :int i) value))
               (egl-check (egl-choose-config (context-display context) attributes config 1 count) 'egl-choose-config)
               (unless (plusp (cffi:mem-ref count :int)) (unavailable 'open-context "No ES3 EGL config"))
               (setf (cffi:mem-aref context-attributes :int 0) #x3098
                     (cffi:mem-aref context-attributes :int 1) 3
                     (cffi:mem-aref context-attributes :int 2) #x3038
                     (context-native context)
                     (egl-check (egl-create-context (context-display context)
                                                    (cffi:mem-ref config :pointer)
                                                    (cffi:null-pointer) context-attributes)
                                'egl-create-context)))
             (assert-context-owner context)
             (setf (context-metadata context) (collect-context-info context device))
             (if software
                 (unless (getf (context-metadata context) :software-p)
                   (unavailable 'open-context "Explicit software device was not classified as software"))
                 (require-hardware-context context))
             (gl-check 'open-context)
             (push context *live-contexts*)
             (setf success t)
             context)
        (unless success (release-context-handles context)))))))

(defun open-context (&key software device)
  "Own an offscreen ES3 context. SOFTWARE explicitly selects the software device.
Hardware is mandatory otherwise. DEVICE selects an absolute DRM render node.
No desktop socket, window surface or inherited display handle is used."
  (when (and software device) (unavailable 'open-context "SOFTWARE and DEVICE are mutually exclusive"))
  (restart-case
      (progn
        (ensure-native-libraries)
        (if software
            (create-context t nil)
            (let ((nodes (if device (list device) (discover-render-nodes))) (failures nil))
              (dolist (node nodes)
                (handler-case (return-from open-context (create-context nil node))
                  (gpu-cleanup-error (error) (error error))
                  (gpu-unavailable (error) (push (format nil "~A: ~A" node error) failures))))
              (unavailable 'open-context
                           (format nil "No usable hardware render node: ~{~A~^; ~}" (reverse failures))))))
    (retry () :report "Retry native device discovery after repairing device access."
      (open-context :software software :device device))
    (use-device (path) :report "Try a specified absolute DRM render-node pathname."
      (open-context :device path))))

(defgeneric close-context (context))
(defmethod close-context ((context graphics-context))
  (check-context-identity context)
  (restart-case
      (progn
        (unless (context-closed-p context)
          (sb-sys:without-interrupts
            (sb-thread:with-mutex (*context-lock*)
              (release-context-handles context)
              (setf *live-contexts* (delete context *live-contexts*)))))
        t)
    (retry-close () :report "Retry releasing this context's retained native handles."
      (close-context context))))

(defgeneric context-info (context))
(defmethod context-info ((context graphics-context))
  (assert-context-owner context)
  (copy-tree (context-metadata context)))

(export '(graphics-context gpu-ownership-error gpu-software-rejected gpu-cleanup-error
          cleanup-error-context require-hardware-context context-display context-native))
