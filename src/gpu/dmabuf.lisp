;;;; GPU-owned export targets. A target is never written while a compositor owns it.
(in-package #:lispterm.gpu)
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(defstruct dma-target context width height texture framebuffer image
  fourcc modifier fds strides offsets (closed-p nil))

;; Strong ownership is intentional until successful cleanup, even if creation
;; never returned a target to its caller. Access is confined to context owners.
(defvar *dma-pending-targets* (make-hash-table :test #'eq))

(define-condition dma-cleanup-failed (error)
  ((target :initarg :target :reader dma-failed-target)
   (cause :initarg :cause :reader dma-cleanup-cause))
  (:report (lambda (condition stream)
             (format stream "DMA target cleanup pending: ~A" (dma-cleanup-cause condition)))))

(defun dma-cleanup-pending (context)
  "Retry partial target cleanup before destroying its owning context."
  (dolist (target (loop for target being the hash-keys of *dma-pending-targets*
                       when (eq context (dma-target-context target)) collect target))
    (dma-destroy target)))

(defun dma-supported-p (context)
  (assert-context-owner context)
  (let ((extensions (egl-query-string (context-display context) #x3055)))
    (and (extension-present-p "EGL_KHR_image_base" extensions)
         (extension-present-p "EGL_MESA_image_dma_buf_export" extensions))))

(defun dma-close-fd (fd)
  (sb-posix:close fd))

(defun dma-destroy (target)
  "Release an export target, retaining ownership when checked teardown fails."
  (restart-case
      (unless (dma-target-closed-p target)
        (assert-context-owner (dma-target-context target))
        (setf (gethash target *dma-pending-targets*) t)
        ;; Reject stale errors before any resource ownership can change.
        (gl-check 'dma-destroy-entry)
        (loop while (dma-target-fds target) do
          (sb-sys:without-interrupts
            (dma-close-fd (first (dma-target-fds target)))
            (pop (dma-target-fds target))))
        (when (dma-target-image target)
          (sb-sys:without-interrupts
            (egl-check (with-driver-call
                         (cffi:foreign-funcall-pointer (extension-address "eglDestroyImageKHR") ()
                           :pointer (context-display (dma-target-context target))
                           :pointer (dma-target-image target) :unsigned-int)) 'dma-destroy)
            (setf (dma-target-image target) nil)))
        (cffi:with-foreign-object (name :unsigned-int)
          (when (dma-target-framebuffer target)
            (setf (cffi:mem-ref name :unsigned-int) (dma-target-framebuffer target))
            (sb-sys:without-interrupts
              (gl-delete-framebuffers 1 name)
              (gl-check 'dma-destroy-framebuffer)
              (setf (dma-target-framebuffer target) nil)))
          (when (dma-target-texture target)
            (setf (cffi:mem-ref name :unsigned-int) (dma-target-texture target))
            (sb-sys:without-interrupts
              (gl-delete-textures 1 name)
              (gl-check 'dma-destroy-texture)
              (setf (dma-target-texture target) nil))))
        (setf (dma-target-closed-p target) t)
        (remhash target *dma-pending-targets*))
    (retry-dma-destroy () :report "Retry unfinished export-target cleanup."
      (dma-destroy target))))

(defun dma-create (context width height)
  "Allocate a dedicated RGBA8 export target; return actual driver plane metadata."
  (assert-context-owner context)
  (unless (and (integerp width) (integerp height) (<= 1 width 16384)
               (<= 1 height 16384) (<= (* width height) 67108864))
    (unavailable 'dma-create "Unsafe target dimensions"))
  (unless (dma-supported-p context) (unavailable 'dma-create "EGL dma-buf export unavailable"))
  (gl-check 'dma-create-entry)
  (let ((target (make-dma-target :context context :width width :height height)) (ok nil))
    ;; Retain even a successful allocation until a pool adopts it. An exit
    ;; between returning the target and the caller's unwind cannot lose it.
    (setf (gethash target *dma-pending-targets*) t)
    (unwind-protect
         (progn
           (cffi:with-foreign-object (name :unsigned-int)
             (setf (cffi:mem-ref name :unsigned-int) 0)
             (sb-sys:without-interrupts
               (gl-gen-textures 1 name)
               (let ((value (cffi:mem-ref name :unsigned-int)))
                 (when (plusp value) (setf (dma-target-texture target) value)))
               (gl-check 'dma-gen-texture))
             (unless (dma-target-texture target) (unavailable 'dma-gen-texture "Zero texture name"))
             (gl-bind-texture #x0de1 (dma-target-texture target))
             (gl-check 'dma-bind-texture)
             (gl-tex-parameter-i #x0de1 #x2801 #x2600)
             (gl-tex-parameter-i #x0de1 #x2800 #x2600)
             (gl-check 'dma-texture-parameters)
             (gl-tex-image-2d #x0de1 0 #x8058 width height 0 #x1908 #x1401 (cffi:null-pointer))
             (gl-check 'dma-texture-storage)
             (setf (cffi:mem-ref name :unsigned-int) 0)
             (sb-sys:without-interrupts
               (gl-gen-framebuffers 1 name)
               (let ((value (cffi:mem-ref name :unsigned-int)))
                 (when (plusp value) (setf (dma-target-framebuffer target) value)))
               (gl-check 'dma-gen-framebuffer))
             (unless (dma-target-framebuffer target) (unavailable 'dma-gen-framebuffer "Zero framebuffer name"))
             (gl-bind-framebuffer #x8d40 (dma-target-framebuffer target))
             (gl-framebuffer-texture-2d #x8d40 #x8ce0 #x0de1 (dma-target-texture target) 0)
             (gl-check 'dma-attach-texture)
             (let ((status (gl-check-framebuffer-status #x8d40)))
               (gl-check 'dma-framebuffer-status)
               (unless (= #x8cd5 status) (unavailable 'dma-create "Incomplete export FBO"))))
           (cffi:with-foreign-object (attrs :int 3)
             (setf (cffi:mem-aref attrs :int 0) #x30d2 ; EGL_IMAGE_PRESERVED_KHR
                   (cffi:mem-aref attrs :int 1) 1
                   (cffi:mem-aref attrs :int 2) #x3038)
              (sb-sys:without-interrupts
                (let ((image (with-driver-call
                               (cffi:foreign-funcall-pointer (extension-address "eglCreateImageKHR") ()
                                 :pointer (context-display context) :pointer (context-native context)
                                 :unsigned-int #x30b1 :pointer (cffi:make-pointer (dma-target-texture target))
                                 :pointer attrs :pointer))))
                  (when (cffi:null-pointer-p image) (unavailable 'dma-create "eglCreateImageKHR failed"))
                  (setf (dma-target-image target) image))))
           (cffi:with-foreign-objects ((format :int) (planes :int) (modifier :uint64))
             (egl-check (with-driver-call
                          (cffi:foreign-funcall-pointer (extension-address "eglExportDMABUFImageQueryMESA") ()
                            :pointer (context-display context) :pointer (dma-target-image target)
                            :pointer format :pointer planes :pointer modifier :unsigned-int)) 'dma-query)
             (let ((count (cffi:mem-ref planes :int)))
               (unless (<= 1 count 4) (unavailable 'dma-query "Invalid exported plane count"))
               (setf (dma-target-fourcc target) (ldb (byte 32 0) (cffi:mem-ref format :int))
                     (dma-target-modifier target) (cffi:mem-ref modifier :uint64))
               (cffi:with-foreign-objects ((fds :int 4) (strides :int 4) (offsets :int 4))
                 (dotimes (i 4) (setf (cffi:mem-aref fds :int i) -1))
                  ;; The driver writes temporary FD slots. Adopt every returned
                  ;; FD, including partial failure output, before an exit can run
                  ;; cleanup. No second FD owner or temporary close path exists.
                  (sb-sys:without-interrupts
                    (unwind-protect
                         (egl-check (with-driver-call
                                      (cffi:foreign-funcall-pointer (extension-address "eglExportDMABUFImageMESA") ()
                                        :pointer (context-display context) :pointer (dma-target-image target)
                                        :pointer fds :pointer strides :pointer offsets :unsigned-int)) 'dma-export)
                      (setf (dma-target-fds target)
                            (loop for i below 4 for fd = (cffi:mem-aref fds :int i)
                                  when (>= fd 0) collect fd))))
                  (unless (= count (length (dma-target-fds target)))
                    (unavailable 'dma-export "Invalid exported FD count"))
                  (setf (dma-target-strides target)
                        (loop for i below count collect (cffi:mem-aref strides :int i))
                        (dma-target-offsets target)
                        (loop for i below count collect (cffi:mem-aref offsets :int i)))
                  (dolist (fd (dma-target-fds target))
                    (sb-posix:fcntl fd sb-posix:f-setfd 1)) ; Linux FD_CLOEXEC
                  (unless (and (every #'plusp (dma-target-strides target))
                               (every (lambda (x) (not (minusp x))) (dma-target-offsets target)))
                    (unavailable 'dma-export "Invalid exported plane layout")))))
           (gl-bind-framebuffer #x8d40 0)
           (gl-check 'dma-unbind-framebuffer)
           (setf ok t) target)
      (unless ok
        (handler-case (dma-destroy target)
          (error (cause)
          (restart-case
              (error 'dma-cleanup-failed :target target :cause cause)
            (retry-dma-construction-cleanup ()
              :report "Retry cleanup of the retained partial export target."
              (dma-destroy target)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(dma-cleanup-failed dma-failed-target dma-cleanup-cause dma-cleanup-pending
            dma-target dma-supported-p dma-create dma-destroy dma-target-context
            dma-target-width dma-target-height dma-target-framebuffer dma-target-fourcc
            dma-target-modifier dma-target-fds dma-target-strides dma-target-offsets)))

(in-package #:lispterm.display.wayland)
(defstruct dma-protocol id formats buffers disabled-reason
  (exports 0) (imports 0) (presents 0) (releases 0) (reuses 0) (finishes 0))
(defstruct dma-buffer id target generation params (params-created-p nil)
  (params-destroyed-p nil) (import-pending-p nil) (complete-p nil)
  (busy-p nil) (uses 0) (protocol-destroyed-p nil))
(defvar *dma-protocols* (make-hash-table :test #'eq :weakness :key))
(defvar *dma-force-unsupported* nil)

(defun dma-global (state name interface version)
  (when (and (string= interface "zwp_linux_dmabuf_v1") (>= version 3))
    (let ((dma (make-dma-protocol)))
      (setf (gethash state *dma-protocols*) dma
            (dma-protocol-id dma)
            (registry-bind state name interface 3
              (lambda (id opcode payload)
                (declare (ignore id))
                (case opcode
                  (0 (when (= (length payload) 4)
                       (pushnew (list (bytes-u32 payload 0) #x00ffffffffffffff)
                                (dma-protocol-formats dma) :test #'equal)))
                  (1 (unless (= (length payload) 12) (wire-error "invalid dma-buf modifier event" nil))
                     (pushnew (list (bytes-u32 payload 0)
                                    (logior (ash (bytes-u32 payload 4) 32)
                                            (bytes-u32 payload 8)))
                              (dma-protocol-formats dma) :test #'equal)))))))))

(defun dma-params-destroy (state buffer &optional disconnected)
  (unless (dma-buffer-params-destroyed-p buffer)
    (sb-sys:without-interrupts
      (when (and (dma-buffer-params-created-p buffer) (not disconnected))
        (wire-send (wayland-state-wire state) (dma-buffer-params buffer) 0))
      (setf (dma-buffer-params-destroyed-p buffer) t))))

(defun dma-buffer-destroy (state buffer &optional disconnected)
  ;; An interrupted roundtrip may leave the server-created buffer ID in
  ;; flight. Retain this owner until its callback records that ID or failure.
  (when (and (dma-buffer-import-pending-p buffer) (not disconnected))
    (protocol-roundtrip state)
    (when (dma-buffer-import-pending-p buffer)
      (wire-error "dma import result still pending at cleanup" nil)))
  (dma-params-destroy state buffer disconnected)
  (unless (dma-buffer-protocol-destroyed-p buffer)
    (sb-sys:without-interrupts
      (unless (or disconnected (null (dma-buffer-id buffer)))
        (wire-send (wayland-state-wire state) (dma-buffer-id buffer) 0))
      (setf (dma-buffer-protocol-destroyed-p buffer) t)))
  (lispterm.gpu:dma-destroy (dma-buffer-target buffer)))

(defun dma-import (state dma target)
  (let* ((wire (wayland-state-wire state)) (failed nil)
         (buffer (make-dma-buffer :target target :generation (wayland-state-generation state))))
    ;; Publish one durable owner before any protocol request or callback. The
    ;; caller tests pool membership on unwind, never normal-return timing.
    (sb-sys:without-interrupts
      (push buffer (dma-protocol-buffers dma))
      (remhash target lispterm.gpu::*dma-pending-targets*))
    (let ((params (wire-id wire :dma-params
                    (lambda (id opcode payload)
                      (declare (ignore id))
                      (case opcode
                        (0 (unless (= 4 (length payload)) (wire-error "invalid dma-buf created event" nil))
                           (sb-sys:without-interrupts
                             (setf (dma-buffer-id buffer) (bytes-u32 payload 0)
                                   (dma-buffer-import-pending-p buffer) nil)))
                        (1 (setf failed t (dma-buffer-import-pending-p buffer) nil)))))))
      (setf (dma-buffer-params buffer) params)
      (unwind-protect
           (progn
             (sb-sys:without-interrupts
               (wire-send wire (dma-protocol-id dma) 1 params)
               (setf (dma-buffer-params-created-p buffer) t))
           (loop for fd in (lispterm.gpu:dma-target-fds target)
                 for stride in (lispterm.gpu:dma-target-strides target)
                 for offset in (lispterm.gpu:dma-target-offsets target)
                 for plane from 0 do
                   (wire-send wire params 1 (list :fd fd) plane offset stride
                              (ldb (byte 32 32) (lispterm.gpu:dma-target-modifier target))
                              (ldb (byte 32 0) (lispterm.gpu:dma-target-modifier target))))
             (sb-sys:without-interrupts
               (wire-send wire params 2 (lispterm.gpu:dma-target-width target)
                          (lispterm.gpu:dma-target-height target)
                          (lispterm.gpu:dma-target-fourcc target) 0)
               (setf (dma-buffer-import-pending-p buffer) t))
             (protocol-roundtrip state)
             (when (or failed (not (dma-buffer-id buffer))) (return-from dma-import nil))
             (setf (gethash (dma-buffer-id buffer) (wire-interfaces wire)) :buffer
                   (gethash (dma-buffer-id buffer) (wire-handlers wire))
                   (lambda (id opcode payload)
                     (declare (ignore id))
                     (unless (and (= opcode 0) (zerop (length payload)))
                       (wire-error "invalid dma wl_buffer.release" nil))
                     (unless (dma-buffer-busy-p buffer) (wire-error "duplicate dma release" nil))
                     (setf (dma-buffer-busy-p buffer) nil)
                     (incf (dma-protocol-releases dma))))
             (incf (dma-protocol-imports dma)))
        (dma-params-destroy state buffer))
      (setf (dma-buffer-complete-p buffer) t)
      buffer)))

(defun dma-collect (state dma)
  (setf (dma-protocol-buffers dma)
        (delete-if (lambda (b)
                      (when (and (not (dma-buffer-busy-p b))
                                 (or (not (dma-buffer-complete-p b))
                                     (/= (dma-buffer-generation b) (wayland-state-generation state))))
                       (dma-buffer-destroy state b) t))
                   (dma-protocol-buffers dma))))

(defun dma-commit (state buffer width height)
  (let* ((wire (wayland-state-wire state))
         (request (wayland-state-geometry-request state))
         (callback (wire-id wire :callback
                     (lambda (id opcode payload)
                       (declare (ignore opcode payload))
                       (remhash id (wire-handlers wire))
                       (setf (wayland-state-frame-pending-p state) nil)
                       (incf (wayland-state-frame-generation state))
                       (setf (wayland-state-frame-request-id state)
                             (and request (lispterm.display:geometry-request-id request))
                             (wayland-state-frame-epoch state)
                             (and request (lispterm.display::geometry-request-epoch request)))
                       (protocol-geometry-frame state request width height)))))
    (wire-send wire (wayland-state-surface state) 3 callback)
    (wire-send wire (wayland-state-surface state) 8 1)
    (wire-send wire (wayland-state-xdg-surface state) 3 0 0 width height)
    (wire-send wire (wayland-state-surface state) 1 (dma-buffer-id buffer) 0 0)
    (wire-send wire (wayland-state-surface state) 2 0 0 width height)
    (setf (dma-buffer-busy-p buffer) t)
    (wire-send wire (wayland-state-surface state) 6)
    (incf (wayland-state-commit-generation state))
    (setf (wayland-state-committed-width state) width
          (wayland-state-committed-height state) height
          (wayland-state-committed-buffer-width state) width
          (wayland-state-committed-buffer-height state) height
          (wayland-state-committed-request-id state)
          (and request (lispterm.display:geometry-request-id request))
          (wayland-state-committed-epoch state)
          (and request (lispterm.display::geometry-request-epoch request))
          (wayland-state-frame-pending-p state) t
          (wayland-state-redraw-p state) nil)))

(defun dma-present (display)
  (let* ((state (window-protocol display)) (dma (gethash state *dma-protocols*)))
    (unless (and (eq (lispterm.display::display-renderer-kind display) :gpu)
                 dma (not *dma-force-unsupported*)
                 (not (dma-protocol-disabled-reason dma))
                 (= 1 (wayland-state-scale state)))
      (return-from dma-present :unsupported))
    (unless (and (wayland-state-configured-p state)
                 (not (wayland-state-frame-pending-p state)))
      (return-from dma-present :busy))
    (dma-collect state dma)
    (let* ((context (lispterm.display::display-renderer-context display))
           (width (lispterm.display:display-width display))
           (height (lispterm.display:display-height display))
           (current (remove-if-not (lambda (b) (= (dma-buffer-generation b)
                                                   (wayland-state-generation state)))
                                   (dma-protocol-buffers dma)))
           (buffer (find-if-not #'dma-buffer-busy-p current)))
      (unless (lispterm.gpu:dma-supported-p context)
        (setf (dma-protocol-disabled-reason dma) :egl-export-unavailable)
        (return-from dma-present :unsupported))
      (unless buffer
        (unless (and (< (length current) 3) (< (length (dma-protocol-buffers dma)) 8)
                     (<= (+ (* width height 4)
                            (loop for b in (dma-protocol-buffers dma)
                                  for target = (dma-buffer-target b)
                                  sum (* 4 (lispterm.gpu:dma-target-width target)
                                           (lispterm.gpu:dma-target-height target)))) 268435456))
          (return-from dma-present :busy))
        (let ((target (lispterm.gpu:dma-create context width height)))
          (unwind-protect
               (progn
                 (incf (dma-protocol-exports dma))
                 (unless (member (list (lispterm.gpu:dma-target-fourcc target)
                                       (lispterm.gpu:dma-target-modifier target))
                                 (dma-protocol-formats dma) :test #'equal)
                   (setf (dma-protocol-disabled-reason dma) :unadvertised-format-modifier)
                   (return-from dma-present :unsupported))
                 (setf buffer (dma-import state dma target))
                 (unless buffer
                   (setf (dma-protocol-disabled-reason dma) :compositor-import-failed)
                    (return-from dma-present :unsupported)))
            (unless (find target (dma-protocol-buffers dma) :key #'dma-buffer-target)
              (lispterm.gpu:dma-destroy target)))))
      (when (dma-buffer-busy-p buffer) (wire-error "attempted in-flight dma overwrite" nil))
      (when (plusp (dma-buffer-uses buffer)) (incf (dma-protocol-reuses dma)))
      (lispterm.gpu:renderer-copy-target (lispterm.display::display-renderer-state display)
                                       (lispterm.gpu:dma-target-framebuffer (dma-buffer-target buffer))
                                       width height :invert-y t)
      ;; Complete GPU writes before compositor import consumption, explicitly.
      (lispterm.gpu:gl-finish)
      (lispterm.gpu::gl-check 'dma-finish)
      (incf (dma-protocol-finishes dma))
      (dma-commit state buffer width height)
      (incf (dma-buffer-uses buffer))
      (incf (dma-protocol-presents dma)) :presented)))

(defun dma-close-display (display)
  ;; Backend disconnect occurs first, invalidating every compositor reference.
  (let* ((state (window-protocol display)) (dma (gethash state *dma-protocols*)))
    (let ((context (lispterm.display::display-renderer-context display)))
      (when context (lispterm.gpu::dma-cleanup-pending context)))
    (when dma
      (loop while (dma-protocol-buffers dma) do
        (dma-buffer-destroy state (first (dma-protocol-buffers dma)) t)
        (pop (dma-protocol-buffers dma)))
      (remhash state *dma-protocols*))))

(defun dma-metrics (state)
  (let ((dma (gethash state *dma-protocols*)))
    (when dma
      (list :exports (dma-protocol-exports dma) :imports (dma-protocol-imports dma)
            :presents (dma-protocol-presents dma) :releases (dma-protocol-releases dma)
            :reuses (dma-protocol-reuses dma) :gpu-finishes (dma-protocol-finishes dma)
            :disabled-reason (dma-protocol-disabled-reason dma)
            :format-modifier-count (length (dma-protocol-formats dma))
            :buffers (length (dma-protocol-buffers dma))))))

(pushnew 'dma-global *protocol-global-hooks*)
(setf *direct-present-hook* #'dma-present *direct-close-hook* #'dma-close-display
      *direct-metrics-hook* #'dma-metrics)
