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

(defun gpu-probe-check ()
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
