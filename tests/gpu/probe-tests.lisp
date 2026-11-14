(in-package #:lispterm.gpu.tests)

(defun probe-program-sources ()
  "A full-target triangle and constant output, authored in the project shader DSL."
  (values
   (lispterm.gpu:emit-shader
    '(:shader :vertex
      (:main
       (:set :position
        (:vec4 (:choose (:= :vertex-id 1) (:float 3) (:float -1))
               (:choose (:= :vertex-id 2) (:float 3) (:float -1))
               (:float 0) (:float 1))))))
   (lispterm.gpu:emit-shader
    '(:shader :fragment
      (:output :vec4 :o-color 0)
      (:main (:set :o-color
              (:vec4 (:/ (:float 17) (:float 255))
                     (:/ (:float 34) (:float 255))
                     (:/ (:float 51) (:float 255)) (:float 1))))))))

(defun assert-offscreen-draw (context)
  "Compile/link real shaders, draw to RGBA8, and check every downloaded texel."
  (lispterm.gpu:assert-context-owner context)
  (let ((program 0) (cell-program 0))
    (cffi:with-foreign-objects ((texture :unsigned-int) (framebuffer :unsigned-int)
                                (vao :unsigned-int) (pixels :unsigned-char 64))
      (setf (cffi:mem-ref texture :unsigned-int) 0
            (cffi:mem-ref framebuffer :unsigned-int) 0
            (cffi:mem-ref vao :unsigned-int) 0)
      (unwind-protect
           (progn
             ;; The production shader pair must compile and link as ES3 too.
             (setf cell-program
                   (lispterm.gpu:link-gl-program
                    (lispterm.gpu:emit-shader (lispterm.gpu:shader-form :vertex))
                    (lispterm.gpu:emit-shader (lispterm.gpu:shader-form :fragment))))
             (multiple-value-bind (vertex fragment) (probe-program-sources)
               (setf program (lispterm.gpu:link-gl-program vertex fragment)))
             (lispterm.gpu:gl-gen-textures 1 texture)
             (lispterm.gpu:gl-bind-texture #x0DE1 (cffi:mem-ref texture :unsigned-int))
             (lispterm.gpu:gl-tex-parameter-i #x0DE1 #x2801 #x2600)
             (lispterm.gpu:gl-tex-parameter-i #x0DE1 #x2800 #x2600)
             (lispterm.gpu:gl-tex-image-2d #x0DE1 0 #x8058 4 4 0 #x1908 #x1401 (cffi:null-pointer))
             (lispterm.gpu:gl-gen-framebuffers 1 framebuffer)
             (lispterm.gpu:gl-bind-framebuffer #x8D40 (cffi:mem-ref framebuffer :unsigned-int))
             (lispterm.gpu:gl-framebuffer-texture-2d #x8D40 #x8CE0 #x0DE1 (cffi:mem-ref texture :unsigned-int) 0)
             (assert (= #x8CD5 (lispterm.gpu:gl-check-framebuffer-status #x8D40)))
             (lispterm.gpu:gl-gen-vertex-arrays 1 vao)
             (lispterm.gpu:gl-bind-vertex-array (cffi:mem-ref vao :unsigned-int))
             (lispterm.gpu:gl-disable #x0BD0) ; dither
             (lispterm.gpu:gl-disable #x0BE2) ; blend
             (lispterm.gpu:gl-disable #x0C11) ; scissor
             (lispterm.gpu:gl-viewport 0 0 4 4)
             (lispterm.gpu:gl-clear-color 1.0 0.0 0.0 0.0)
             (lispterm.gpu:gl-clear #x4000)
             (lispterm.gpu:gl-use-program program)
             (lispterm.gpu:gl-draw-arrays #x0004 0 3)
             (lispterm.gpu:gl-pixel-store-i #x0D05 1)
             (lispterm.gpu:gl-read-pixels 0 0 4 4 #x1908 #x1401 pixels)
             (lispterm.gpu:gl-finish)
             (lispterm.gpu:gl-check 'probe-draw)
             (dotimes (pixel 16)
               (loop for expected in '(17 34 51 255) for channel from 0
                     do (assert (= expected (cffi:mem-aref pixels :unsigned-char
                                                          (+ (* pixel 4) channel))))))
             t)
        (lispterm.gpu:gl-use-program 0)
        (lispterm.gpu:gl-bind-framebuffer #x8D40 0)
        (lispterm.gpu:gl-bind-vertex-array 0)
        (lispterm.gpu:gl-delete-vertex-arrays 1 vao)
        (lispterm.gpu:gl-delete-framebuffers 1 framebuffer)
        (lispterm.gpu:gl-delete-textures 1 texture)
        (unless (zerop program) (lispterm.gpu:gl-delete-program program))
        (unless (zerop cell-program) (lispterm.gpu:gl-delete-program cell-program))))))

(defun assert-owner-guards (context)
  (let ((result
          (sb-thread:join-thread
           (sb-thread:make-thread
            (lambda ()
              (handler-case (progn (lispterm.gpu:assert-context-owner context) :unexpected)
                (lispterm.gpu:gpu-ownership-error () :rejected)))))))
    (assert (eq :rejected result)))
  ;; Simulate an inherited object's pid without forking a driver-owning process.
  (let ((original (slot-value context 'lispterm.gpu::pid)))
    (unwind-protect
         (progn
           (setf (slot-value context 'lispterm.gpu::pid) -1)
           (assert
            (handler-case (progn (lispterm.gpu:assert-context-owner context) nil)
              (lispterm.gpu:gpu-ownership-error () t))))
      (setf (slot-value context 'lispterm.gpu::pid) original)))
  (assert
   (handler-case (progn (lispterm.gpu::require-no-live-contexts) nil)
     (lispterm.gpu:gpu-unavailable () t)))
  t)

(defun call-with-driver-control (symbol replacement function)
  ;; A disposable test process only. Restore even when an assertion unwinds.
  (let ((original (symbol-function symbol)))
    (unwind-protect
         (progn (setf (symbol-function symbol) replacement) (funcall function))
      (setf (symbol-function symbol) original))))

(defun assert-context-cleanup-controls ()
  ;; Inject one failure at each fallible EGL release. The retained object must
  ;; be reachable and retryable, and already-destroyed handles must be cleared.
  (dolist (stage '(lispterm.gpu:egl-make-current lispterm.gpu:egl-destroy-context
                   lispterm.gpu:egl-terminate))
    (let ((context (lispterm.gpu:open-context)))
      (unwind-protect
           (progn
             (call-with-driver-control
              stage (lambda (&rest arguments) (declare (ignore arguments)) 0)
              (lambda ()
                (let ((condition
                        (handler-case (progn (lispterm.gpu:close-context context) nil)
                          (lispterm.gpu:gpu-cleanup-error (condition) condition))))
                  (assert condition)
                  (assert (eq context (lispterm.gpu:cleanup-error-context condition)))
                  (assert (member context lispterm.gpu::*live-contexts*))
                  (assert (not (lispterm.gpu::context-closed-p context)))
                  (assert (eq (eq stage 'lispterm.gpu:egl-terminate)
                              (cffi:null-pointer-p (lispterm.gpu:context-native context)))))))
             (assert (lispterm.gpu:close-context context))
             (assert (not (member context lispterm.gpu::*live-contexts*))))
        (lispterm.gpu:close-context context))))
  ;; Fail after acquiring all native resources. Successful cleanup must remove
  ;; the device FD too, and a fresh hardware context must still open/draw.
  (let ((before (length (uiop:directory-files #p"/proc/self/fd/"))))
    (call-with-driver-control
     'lispterm.gpu::collect-context-info
     (lambda (&rest arguments) (declare (ignore arguments)) (error "Injected metadata failure"))
     (lambda ()
       (assert (handler-case (progn (lispterm.gpu:open-context) nil)
                 (simple-error () t)))))
    (assert (null lispterm.gpu::*live-contexts*))
    (assert (= before (length (uiop:directory-files #p"/proc/self/fd/")))))
  ;; Failed-creation cleanup itself can fail. Its typed condition must carry the
  ;; partially created context instead of silently trying another DRM device.
  (let ((retained nil))
    (unwind-protect
         (catch 'metadata-control
           (call-with-driver-control
            'lispterm.gpu::collect-context-info
            (lambda (&rest arguments) (declare (ignore arguments)) (throw 'metadata-control nil))
            (lambda ()
              (call-with-driver-control
               'lispterm.gpu:egl-destroy-context
               (lambda (&rest arguments) (declare (ignore arguments)) 0)
               (lambda ()
                 (handler-case (lispterm.gpu:open-context)
                   (lispterm.gpu:gpu-cleanup-error (condition)
                     (setf retained (lispterm.gpu:cleanup-error-context condition)))))))))
      (when retained (lispterm.gpu:close-context retained)))
    (assert retained))
  (assert (null lispterm.gpu::*live-contexts*))
  (format t "CONTEXT-CLEANUP injected unbind/destroy/terminate and partial-create failures: retained handles, typed errors and retry verified~%")
  t)

(defun mismatched-program-sources ()
  (values
   (lispterm.gpu:emit-shader
    '(:shader :vertex (:output :vec4 :v 0)
      (:main (:set :position (:vec4 (:float 0)))
             (:set :v (:vec4 (:float 1))))))
   (lispterm.gpu:emit-shader
    '(:shader :fragment (:input :vec3 :v 0) (:output :vec4 :color 0)
      (:main (:set :color (:vec4 :v (:float 1))))))))

(defun assert-gl-construction-controls (context)
  ;; These are real GLES errors, not Lisp-only simulated exceptions.
  (dolist (operation
           (list (lambda () (lispterm.gpu:compile-gl-shader 0 ""))
                 (lambda () (lispterm.gpu:compile-gl-shader #x8B31 ""))
                 (lambda ()
                   (multiple-value-bind (vertex fragment) (mismatched-program-sources)
                     (lispterm.gpu:link-gl-program vertex fragment)))
                 (lambda () (lispterm.gpu::gl-object-status :shader 0 #x8B81))
                 (lambda () (lispterm.gpu::gl-object-status :program 0 #x8B82))
                 (lambda () (lispterm.gpu::gl-object-log :shader 0))
                 (lambda () (lispterm.gpu::gl-object-log :program 0))))
    (assert (handler-case (progn (funcall operation) nil)
              (lispterm.gpu:gpu-unavailable (condition)
                (assert (plusp (length (princ-to-string condition)))) t)))
    (assert (zerop (lispterm.gpu:gl-get-error)))
    (assert (null lispterm.gpu::*pending-gl-objects*)))
  ;; Failure after a real allocation and a real invalid-object deletion must
  ;; retain ownership. Ordinary CLOSE-CONTEXT drains it before EGL teardown.
  (dolist (kind '(:shader :program))
    (let* ((temporary (lispterm.gpu:open-context))
           (symbol (ecase kind (:shader 'lispterm.gpu:gl-delete-shader)
                               (:program 'lispterm.gpu:gl-delete-program)))
           (original (symbol-function symbol))
           (native (lispterm.gpu:context-native temporary))
           (fd (lispterm.gpu::context-fd temporary))
           (retained nil))
      (unwind-protect
           (call-with-driver-control
            symbol
            (lambda (name) (declare (ignore name)) (funcall original #xffffffff))
            (lambda ()
              (setf retained
                    (handler-case
                        (ecase kind
                          (:shader (lispterm.gpu:compile-gl-shader #x8B31 ""))
                          (:program
                           (multiple-value-bind (vertex fragment) (mismatched-program-sources)
                             (lispterm.gpu:link-gl-program vertex fragment))))
                      (lispterm.gpu:gl-object-cleanup-error (condition) condition)))
              (assert (typep retained 'lispterm.gpu:gl-object-cleanup-error))
              (assert (not (typep retained 'lispterm.gpu:gpu-unavailable)))
              (assert (lispterm.gpu:gl-cleanup-objects retained))
              (assert (handler-case (progn (lispterm.gpu:close-context temporary) nil)
                        (lispterm.gpu:gl-object-cleanup-error () t)))
              (assert (cffi:pointer-eq native (lispterm.gpu:context-native temporary)))
              (assert (= fd (lispterm.gpu::context-fd temporary)))
              (assert (member temporary lispterm.gpu::*live-contexts*))
              (assert (not (lispterm.gpu::context-closed-p temporary)))
              (assert (handler-case (progn (lispterm.gpu::require-no-live-contexts) nil)
                        (lispterm.gpu:gpu-unavailable () t)))))
        (lispterm.gpu:close-context temporary)
        (lispterm.gpu:assert-context-owner context))
      (assert (null lispterm.gpu::*pending-gl-objects*))
      (assert (every (lambda (owner) (zerop (lispterm.gpu::gl-object-owner-name owner)))
                     (lispterm.gpu:gl-cleanup-objects retained)))))
  ;; An arbitrary non-error exit must still release the real allocated shader.
  (assert (eq :escaped
              (catch 'gl-escape
                (lispterm.gpu::call-with-gl-construction
                 (lambda (own)
                   (funcall own :shader (lispterm.gpu:gl-create-shader #x8B31))
                   (throw 'gl-escape :escaped))))))
  (assert (null lispterm.gpu::*pending-gl-objects*))
  (lispterm.gpu:gl-check 'construction-controls)
  (format t "GL-CONSTRUCTION invalid stage/status/log, compile/link failure, real deletion errors, retained context/save guard, close retry and nonlocal exit passed~%")
  t)

(defun assert-gl-auto-cleanup-control (context)
  (let* ((display (lispterm.display::make-display
                   :owner sb-thread:*current-thread* :renderer-request :auto
                   :font (lispterm.display::load-font nil 16) :width 16 :height 16))
         (compile (symbol-function 'lispterm.gpu:compile-gl-shader))
         (delete (symbol-function 'lispterm.gpu:gl-delete-shader))
         (close (symbol-function 'lispterm.gpu:close-context))
         (closes 0) (retained nil))
    (unwind-protect
         (call-with-driver-control
          'lispterm.gpu:compile-gl-shader
          (lambda (stage source) (declare (ignore stage source))
            (funcall compile #x8B31 ""))
          (lambda ()
            (call-with-driver-control
             'lispterm.gpu:gl-delete-shader
             (lambda (name) (declare (ignore name)) (funcall delete #xffffffff))
             (lambda ()
               (call-with-driver-control
                'lispterm.gpu:close-context
                (lambda (owner) (incf closes) (funcall close owner))
                (lambda ()
                  (setf retained
                        (handler-case
                            (progn (lispterm.display::initialize-display-renderer display) nil)
                          (lispterm.gpu:gl-object-cleanup-error (condition) condition)))
                  (assert retained)
                  (assert (zerop closes))
                  (assert (not (lispterm.display::gpu-unavailable-p retained)))
                  (assert (null (lispterm.display::display-renderer-fallback-reason display)))
                  (assert (member (lispterm.display::display-renderer-context display)
                                  lispterm.gpu::*live-contexts*))
                  (assert lispterm.gpu::*pending-gl-objects*)))))))
      (lispterm.display::close-display-renderer display)
      (lispterm.gpu:assert-context-owner context))
    (assert (null lispterm.gpu::*pending-gl-objects*))
    (format t "GL-AUTO real shader compile/deletion failure: no implicit context close/fallback; ordinary close drained retained objects~%")
    t))

(defun gpu-probe-check ()
  (assert (assert-context-cleanup-controls))
  (let ((context (lispterm.gpu:open-context)))
    (unwind-protect
         (progn
           (let ((info (lispterm.gpu:context-info context)))
             (assert (not (getf info :software-p)))
             (assert (getf info :device-associated-p))
             (assert (probe-file (getf info :device)))
             (assert (lispterm.gpu:require-hardware-context context))
             (format t "HARDWARE-CONTEXT ~S~%" info))
           (assert (assert-owner-guards context))
             (assert (assert-gl-construction-controls context))
             (assert (assert-gl-auto-cleanup-control context))
           (assert (assert-offscreen-draw context)))
      (lispterm.gpu:close-context context))
    (assert (lispterm.gpu:close-context context))
    (assert
     (handler-case (progn (lispterm.gpu:assert-context-owner context) nil)
       (lispterm.gpu:gpu-ownership-error () t))))
  (assert (null lispterm.gpu::*live-contexts*))
  (format t "HARDWARE-DRAW 4x4 RGBA8, all 64 channels exact; production shaders linked; ownership/close verified~%")
  t)

(register-check "gpu-probe" #'gpu-probe-check)
