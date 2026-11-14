;;;; GPU-owned export targets. A target is never written while a compositor owns it.
(in-package #:lispterm.gpu)
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(defstruct dma-target context width height texture framebuffer image
  fourcc modifier fds strides offsets (closed-p nil))

(defun dma-supported-p (context)
  (assert-context-owner context)
  (let ((extensions (egl-query-string (context-display context) #x3055)))
    (and (extension-present-p "EGL_KHR_image_base" extensions)
         (extension-present-p "EGL_MESA_image_dma_buf_export" extensions))))

(defun dma-destroy (target)
  "Release an export target, retaining ownership when checked teardown fails."
  (restart-case
      (unless (dma-target-closed-p target)
        (assert-context-owner (dma-target-context target))
        ;; Reject stale errors before any resource ownership can change.
        (gl-check 'dma-destroy-entry)
        (loop while (dma-target-fds target) do
          (sb-sys:without-interrupts
            (sb-posix:close (first (dma-target-fds target)))
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
        (setf (dma-target-closed-p target) t))
    (retry-dma-destroy () :report "Retry unfinished export-target cleanup."
      (dma-destroy target))))

(defun dma-create (context width height)
  "Allocate a dedicated RGBA8 export target; return actual driver plane metadata."
  (assert-context-owner context)
  (unless (and (integerp width) (integerp height) (<= 1 width 16384)
               (<= 1 height 16384) (<= (* width height) 67108864))
    (unavailable 'dma-create "Unsafe target dimensions"))
  (unless (dma-supported-p context) (unavailable 'dma-create "EGL dma-buf export unavailable"))
  (let ((target (make-dma-target :context context :width width :height height)) (ok nil))
    (unwind-protect
         (progn
           (cffi:with-foreign-object (name :unsigned-int)
             (gl-gen-textures 1 name)
             (setf (dma-target-texture target) (cffi:mem-ref name :unsigned-int))
             (gl-bind-texture #x0de1 (dma-target-texture target))
             (gl-tex-parameter-i #x0de1 #x2801 #x2600)
             (gl-tex-parameter-i #x0de1 #x2800 #x2600)
             (gl-tex-image-2d #x0de1 0 #x8058 width height 0 #x1908 #x1401 (cffi:null-pointer))
             (gl-gen-framebuffers 1 name)
             (setf (dma-target-framebuffer target) (cffi:mem-ref name :unsigned-int))
             (gl-bind-framebuffer #x8d40 (dma-target-framebuffer target))
             (gl-framebuffer-texture-2d #x8d40 #x8ce0 #x0de1 (dma-target-texture target) 0)
             (unless (= #x8cd5 (gl-check-framebuffer-status #x8d40))
               (unavailable 'dma-create "Incomplete export FBO")))
           (cffi:with-foreign-object (attrs :int 3)
             (setf (cffi:mem-aref attrs :int 0) #x30d2 ; EGL_IMAGE_PRESERVED_KHR
                   (cffi:mem-aref attrs :int 1) 1
                   (cffi:mem-aref attrs :int 2) #x3038)
             (let ((image (with-driver-call
                            (cffi:foreign-funcall-pointer (extension-address "eglCreateImageKHR") ()
                              :pointer (context-display context) :pointer (context-native context)
                              :unsigned-int #x30b1 :pointer (cffi:make-pointer (dma-target-texture target))
                              :pointer attrs :pointer))))
               (when (cffi:null-pointer-p image) (unavailable 'dma-create "eglCreateImageKHR failed"))
               (setf (dma-target-image target) image)))
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
                 (unwind-protect
                      (progn
                        (egl-check (with-driver-call
                                     (cffi:foreign-funcall-pointer (extension-address "eglExportDMABUFImageMESA") ()
                                       :pointer (context-display context) :pointer (dma-target-image target)
                                       :pointer fds :pointer strides :pointer offsets :unsigned-int)) 'dma-export)
                        (setf (dma-target-fds target)
                              (loop for i below count collect (cffi:mem-aref fds :int i))
                              (dma-target-strides target)
                              (loop for i below count collect (cffi:mem-aref strides :int i))
                              (dma-target-offsets target)
                              (loop for i below count collect (cffi:mem-aref offsets :int i)))
                        (dotimes (i count) (setf (cffi:mem-aref fds :int i) -1))
                        (dolist (fd (dma-target-fds target))
                          (when (minusp fd) (unavailable 'dma-export "Invalid exported FD"))
                          (sb-posix:fcntl fd sb-posix:f-setfd 1)) ; Linux FD_CLOEXEC
                        (unless (and (every #'plusp (dma-target-strides target))
                                     (every (lambda (x) (not (minusp x))) (dma-target-offsets target)))
                          (unavailable 'dma-export "Invalid exported plane layout")))
                   (dotimes (i 4) (let ((fd (cffi:mem-aref fds :int i)))
                                   (when (>= fd 0) (sb-posix:close fd))))))))
           (gl-bind-framebuffer #x8d40 0)
           (setf ok t) target)
      (unless ok (dma-destroy target)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(dma-target dma-supported-p dma-create dma-destroy dma-target-context
            dma-target-width dma-target-height dma-target-framebuffer dma-target-fourcc
            dma-target-modifier dma-target-fds dma-target-strides dma-target-offsets)))

(in-package #:lispterm.display.wayland)
(defstruct dma-protocol id formats buffers disabled-reason
  (exports 0) (imports 0) (presents 0) (releases 0) (reuses 0) (finishes 0))
(defstruct dma-buffer id target generation (busy-p nil) (uses 0))
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

(defun dma-buffer-destroy (state buffer &optional disconnected)
  (unless disconnected (wire-send (wayland-state-wire state) (dma-buffer-id buffer) 0))
  (lispterm.gpu:dma-destroy (dma-buffer-target buffer)))

(defun dma-import (state dma target)
  (let* ((wire (wayland-state-wire state)) (result nil) (failed nil)
         (buffer (make-dma-buffer :target target :generation (wayland-state-generation state)))
         (params (wire-id wire :dma-params
                   (lambda (id opcode payload)
                     (declare (ignore id))
                     (case opcode
                       (0 (unless (= 4 (length payload)) (wire-error "invalid dma-buf created event" nil))
                          (setf result (bytes-u32 payload 0)))
                       (1 (setf failed t)))))))
    (unwind-protect
         (progn
           (wire-send wire (dma-protocol-id dma) 1 params)
           (loop for fd in (lispterm.gpu:dma-target-fds target)
                 for stride in (lispterm.gpu:dma-target-strides target)
                 for offset in (lispterm.gpu:dma-target-offsets target)
                 for plane from 0 do
                   (wire-send wire params 1 (list :fd fd) plane offset stride
                              (ldb (byte 32 32) (lispterm.gpu:dma-target-modifier target))
                              (ldb (byte 32 0) (lispterm.gpu:dma-target-modifier target))))
           (wire-send wire params 2 (lispterm.gpu:dma-target-width target)
                      (lispterm.gpu:dma-target-height target)
                      (lispterm.gpu:dma-target-fourcc target) 0)
           (protocol-roundtrip state)
           (when (or failed (not result)) (return-from dma-import nil))
           (setf (dma-buffer-id buffer) result
                 (gethash result (wire-interfaces wire)) :buffer
                 (gethash result (wire-handlers wire))
                 (lambda (id opcode payload)
                   (declare (ignore id))
                   (unless (and (= opcode 0) (zerop (length payload)))
                     (wire-error "invalid dma wl_buffer.release" nil))
                   (unless (dma-buffer-busy-p buffer) (wire-error "duplicate dma release" nil))
                   (setf (dma-buffer-busy-p buffer) nil)
                   (incf (dma-protocol-releases dma))))
           (incf (dma-protocol-imports dma))
           (push buffer (dma-protocol-buffers dma)) buffer)
      (wire-send wire params 0))))

(defun dma-collect (state dma)
  (setf (dma-protocol-buffers dma)
        (delete-if (lambda (b)
                     (when (and (not (dma-buffer-busy-p b))
                                (/= (dma-buffer-generation b) (wayland-state-generation state)))
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
        (let ((target (lispterm.gpu:dma-create context width height)) (owned nil))
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
                   (return-from dma-present :unsupported))
                 (setf owned t))
            (unless owned (lispterm.gpu:dma-destroy target)))))
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
