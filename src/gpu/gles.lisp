(in-package #:lispterm.gpu)

;;; Raw ES3 entrypoints: integer enums follow Khronos GLES3/gl3.h. Pointer
;;; storage is caller-owned and valid for the duration of the call only.
(define-driver-function gl-get-string :gles "glGetString" :string (name :unsigned-int))
(define-driver-function gl-get-string-i :gles "glGetStringi" :string (name :unsigned-int) (index :unsigned-int))
(define-driver-function gl-get-integer-v :gles "glGetIntegerv" :void (name :unsigned-int) (value :pointer))
(define-driver-function gl-get-error :gles "glGetError" :unsigned-int)
(define-driver-function gl-finish :gles "glFinish" :void)
(define-driver-function gl-flush :gles "glFlush" :void)
(define-driver-function gl-viewport :gles "glViewport" :void (x :int) (y :int) (width :int) (height :int))
(define-driver-function gl-clear-color :gles "glClearColor" :void (r :float) (g :float) (b :float) (a :float))
(define-driver-function gl-clear :gles "glClear" :void (mask :unsigned-int))
(define-driver-function gl-enable :gles "glEnable" :void (cap :unsigned-int))
(define-driver-function gl-disable :gles "glDisable" :void (cap :unsigned-int))
(define-driver-function gl-scissor :gles "glScissor" :void (x :int) (y :int) (width :int) (height :int))
(define-driver-function gl-blend-func :gles "glBlendFunc" :void (source :unsigned-int) (dest :unsigned-int))
(define-driver-function gl-create-shader :gles "glCreateShader" :unsigned-int (type :unsigned-int))
(define-driver-function gl-shader-source :gles "glShaderSource" :void (shader :unsigned-int) (count :int) (strings :pointer) (lengths :pointer))
(define-driver-function gl-compile-shader :gles "glCompileShader" :void (shader :unsigned-int))
(define-driver-function gl-get-shader-iv :gles "glGetShaderiv" :void (shader :unsigned-int) (name :unsigned-int) (value :pointer))
(define-driver-function gl-get-shader-info-log :gles "glGetShaderInfoLog" :void (shader :unsigned-int) (size :int) (length :pointer) (log :pointer))
(define-driver-function gl-delete-shader :gles "glDeleteShader" :void (shader :unsigned-int))
(define-driver-function gl-create-program :gles "glCreateProgram" :unsigned-int)
(define-driver-function gl-attach-shader :gles "glAttachShader" :void (program :unsigned-int) (shader :unsigned-int))
(define-driver-function gl-link-program :gles "glLinkProgram" :void (program :unsigned-int))
(define-driver-function gl-get-program-iv :gles "glGetProgramiv" :void (program :unsigned-int) (name :unsigned-int) (value :pointer))
(define-driver-function gl-get-program-info-log :gles "glGetProgramInfoLog" :void (program :unsigned-int) (size :int) (length :pointer) (log :pointer))
(define-driver-function gl-delete-program :gles "glDeleteProgram" :void (program :unsigned-int))
(define-driver-function gl-use-program :gles "glUseProgram" :void (program :unsigned-int))
(define-driver-function gl-get-uniform-location :gles "glGetUniformLocation" :int (program :unsigned-int) (name :string))
(define-driver-function gl-uniform-1i :gles "glUniform1i" :void (location :int) (x :int))
(define-driver-function gl-uniform-1f :gles "glUniform1f" :void (location :int) (x :float))
(define-driver-function gl-uniform-2f :gles "glUniform2f" :void (location :int) (x :float) (y :float))
(define-driver-function gl-uniform-2i :gles "glUniform2i" :void (location :int) (x :int) (y :int))
(define-driver-function gl-uniform-4f :gles "glUniform4f" :void (location :int) (x :float) (y :float) (z :float) (w :float))
(define-driver-function gl-gen-textures :gles "glGenTextures" :void (count :int) (names :pointer))
(define-driver-function gl-delete-textures :gles "glDeleteTextures" :void (count :int) (names :pointer))
(define-driver-function gl-active-texture :gles "glActiveTexture" :void (texture :unsigned-int))
(define-driver-function gl-bind-texture :gles "glBindTexture" :void (target :unsigned-int) (texture :unsigned-int))
(define-driver-function gl-tex-parameter-i :gles "glTexParameteri" :void (target :unsigned-int) (name :unsigned-int) (value :int))
(define-driver-function gl-tex-image-2d :gles "glTexImage2D" :void (target :unsigned-int) (level :int) (internal :int) (width :int) (height :int) (border :int) (format :unsigned-int) (type :unsigned-int) (data :pointer))
(define-driver-function gl-tex-sub-image-2d :gles "glTexSubImage2D" :void (target :unsigned-int) (level :int) (x :int) (y :int) (width :int) (height :int) (format :unsigned-int) (type :unsigned-int) (data :pointer))
(define-driver-function gl-pixel-store-i :gles "glPixelStorei" :void (name :unsigned-int) (value :int))
(define-driver-function gl-gen-framebuffers :gles "glGenFramebuffers" :void (count :int) (names :pointer))
(define-driver-function gl-delete-framebuffers :gles "glDeleteFramebuffers" :void (count :int) (names :pointer))
(define-driver-function gl-bind-framebuffer :gles "glBindFramebuffer" :void (target :unsigned-int) (framebuffer :unsigned-int))
(define-driver-function gl-framebuffer-texture-2d :gles "glFramebufferTexture2D" :void (target :unsigned-int) (attachment :unsigned-int) (texture-target :unsigned-int) (texture :unsigned-int) (level :int))
(define-driver-function gl-check-framebuffer-status :gles "glCheckFramebufferStatus" :unsigned-int (target :unsigned-int))
(define-driver-function gl-blit-framebuffer :gles "glBlitFramebuffer" :void
  (sx0 :int) (sy0 :int) (sx1 :int) (sy1 :int)
  (dx0 :int) (dy0 :int) (dx1 :int) (dy1 :int)
  (mask :unsigned-int) (filter :unsigned-int))
(define-driver-function gl-gen-buffers :gles "glGenBuffers" :void (count :int) (names :pointer))
(define-driver-function gl-delete-buffers :gles "glDeleteBuffers" :void (count :int) (names :pointer))
(define-driver-function gl-bind-buffer :gles "glBindBuffer" :void (target :unsigned-int) (buffer :unsigned-int))
(define-driver-function gl-buffer-data :gles "glBufferData" :void (target :unsigned-int) (size :intptr) (data :pointer) (usage :unsigned-int))
(define-driver-function gl-buffer-sub-data :gles "glBufferSubData" :void (target :unsigned-int) (offset :intptr) (size :intptr) (data :pointer))
(define-driver-function gl-gen-vertex-arrays :gles "glGenVertexArrays" :void (count :int) (names :pointer))
(define-driver-function gl-delete-vertex-arrays :gles "glDeleteVertexArrays" :void (count :int) (names :pointer))
(define-driver-function gl-bind-vertex-array :gles "glBindVertexArray" :void (array :unsigned-int))
(define-driver-function gl-enable-vertex-attrib-array :gles "glEnableVertexAttribArray" :void (index :unsigned-int))
(define-driver-function gl-vertex-attrib-i-pointer :gles "glVertexAttribIPointer" :void (index :unsigned-int) (size :int) (type :unsigned-int) (stride :int) (pointer :pointer))
(define-driver-function gl-vertex-attrib-pointer :gles "glVertexAttribPointer" :void (index :unsigned-int) (size :int) (type :unsigned-int) (normalized :unsigned-char) (stride :int) (pointer :pointer))
(define-driver-function gl-vertex-attrib-divisor :gles "glVertexAttribDivisor" :void (index :unsigned-int) (divisor :unsigned-int))
(define-driver-function gl-draw-arrays :gles "glDrawArrays" :void (mode :unsigned-int) (first :int) (count :int))
(define-driver-function gl-draw-arrays-instanced :gles "glDrawArraysInstanced" :void (mode :unsigned-int) (first :int) (count :int) (instances :int))
(define-driver-function gl-read-pixels :gles "glReadPixels" :void (x :int) (y :int) (width :int) (height :int) (format :unsigned-int) (type :unsigned-int) (data :pointer))

(defun gl-integer (name)
  (cffi:with-foreign-object (value :int)
    (gl-get-integer-v name value)
    (cffi:mem-ref value :int)))

(defun gl-integers (name count)
  (check-type count (integer 1 16))
  (cffi:with-foreign-object (values :int count)
    (gl-get-integer-v name values)
    (loop for i below count collect (cffi:mem-aref values :int i))))

(defun gl-check (operation)
  (let ((code (gl-get-error)))
    (unless (zerop code)
      (unavailable operation (format nil "GL error #x~X" code))))
  t)

;;; Failed construction retains each allocation until checked deletion succeeds.
(define-driver-function gl-object-current-context :egl "eglGetCurrentContext" :pointer)

(defstruct gl-object-owner
  kind name context thread pid)

(defvar *pending-gl-objects* nil)
(defvar *gl-object-lock* (sb-thread:make-mutex :name "GL construction ownership"))

(define-condition gl-object-cleanup-error (error)
  ((objects :initarg :objects :reader gl-cleanup-objects)
   (cause :initarg :cause :reader gl-cleanup-cause))
  (:report (lambda (condition stream)
             (format stream "GL construction cleanup pending for ~D object(s): ~A"
                     (length (gl-cleanup-objects condition))
                     (gl-cleanup-cause condition)))))

(defun own-gl-object (kind name)
  (when (plusp name)
    (let ((owner (make-gl-object-owner
                  :kind kind :name name :context (gl-object-current-context)
                  :thread sb-thread:*current-thread* :pid (sb-unix:unix-getpid))))
      (sb-thread:with-mutex (*gl-object-lock*) (push owner *pending-gl-objects*))
      owner)))

(defun forget-gl-object (owner)
  (sb-thread:with-mutex (*gl-object-lock*)
    (setf *pending-gl-objects* (delete owner *pending-gl-objects*))))

(defun pending-gl-objects-p (native-context)
  (sb-thread:with-mutex (*gl-object-lock*)
    (some (lambda (owner)
            (and (= (gl-object-owner-pid owner) (sb-unix:unix-getpid))
                 (cffi:pointer-eq native-context (gl-object-owner-context owner))))
          *pending-gl-objects*)))
(defun delete-owned-gl-object (owner)
  (unless (and (= (gl-object-owner-pid owner) (sb-unix:unix-getpid))
               (eq (gl-object-owner-thread owner) sb-thread:*current-thread*)
               (cffi:pointer-eq (gl-object-owner-context owner) (gl-object-current-context))
               (not (cffi:null-pointer-p (gl-object-owner-context owner))))
    (error "Restore the allocating process, thread and EGL context before GL cleanup"))
  ;; A stale error must never make an already completed deletion look retryable.
  (gl-check 'before-gl-object-delete)
  (ecase (gl-object-owner-kind owner)
    (:shader (gl-delete-shader (gl-object-owner-name owner)))
    (:program (gl-delete-program (gl-object-owner-name owner))))
  (gl-check 'gl-object-delete)
  (setf (gl-object-owner-name owner) 0)
  (forget-gl-object owner))

(defun retry-gl-object-cleanup (condition)
  "Retry retained construction allocations with their allocating context current."
  (let ((remaining nil) (cause nil))
    (dolist (owner (gl-cleanup-objects condition))
      (when (plusp (gl-object-owner-name owner))
        (handler-case (delete-owned-gl-object owner)
          (error (failure) (push owner remaining) (setf cause failure)))))
    (when remaining
      (restart-case
          (error 'gl-object-cleanup-error :objects (nreverse remaining) :cause cause)
        (retry-gl-object-cleanup () :report "Retry checked GL object deletion."
          (retry-gl-object-cleanup condition))))
    t))

(defun drain-gl-object-cleanup ()
  "Delete pending construction objects for the current EGL context before teardown."
  (let ((objects
          (sb-thread:with-mutex (*gl-object-lock*)
            (remove-if-not
             (lambda (owner)
               (and (= (gl-object-owner-pid owner) (sb-unix:unix-getpid))
                    (eq (gl-object-owner-thread owner) sb-thread:*current-thread*)
                    (cffi:pointer-eq (gl-object-owner-context owner)
                                     (gl-object-current-context))))
             *pending-gl-objects*))))
    (retry-gl-object-cleanup
     (make-condition 'gl-object-cleanup-error :objects objects :cause "pending construction"))))

(defun call-with-gl-construction (function)
  "FUNCTION registers allocations and returns the owner to transfer to its caller."
  (sb-sys:without-interrupts
    (let ((owners nil) (result nil) (cause "nonlocal exit") (complete nil))
      (unwind-protect
           (handler-bind ((error (lambda (condition) (setf cause condition))))
             (setf result
                   (funcall function
                            (lambda (kind name)
                              (let ((owner (own-gl-object kind name)))
                                (when owner (push owner owners))
                                owner)))
                   complete t))
        (let ((remaining nil))
          (dolist (owner owners)
            (unless (and complete (eq owner result))
              (handler-case (delete-owned-gl-object owner)
                (error (failure) (setf cause failure) (push owner remaining)))))
          (when remaining
            ;; A successful result cannot be transferred when temporary cleanup failed.
            (when (and complete result) (push result remaining))
            (restart-case
                (error 'gl-object-cleanup-error :objects remaining :cause cause)
              (retry-gl-object-cleanup () :report "Delete retained GL construction objects."
                (retry-gl-object-cleanup
                 (make-condition 'gl-object-cleanup-error :objects remaining :cause cause))))
            ;; Successful retry releases resources, not a program/shader result.
            (error "GL construction aborted after cleanup retry"))))
      (forget-gl-object result)
      (gl-object-owner-name result))))

(defun gl-object-status (kind object parameter)
  (cffi:with-foreign-object (status :int)
    (setf (cffi:mem-ref status :int) 0)
    (ecase kind
      (:shader (gl-get-shader-iv object parameter status))
      (:program (gl-get-program-iv object parameter status)))
    (gl-check 'gl-object-status)
    (cffi:mem-ref status :int)))

(defun gl-object-log (kind object)
  "Read only the checked, bounded byte count actually written by GLES."
  (cffi:with-foreign-objects ((log :unsigned-char 8192) (length :int))
    (dotimes (i 8192) (setf (cffi:mem-aref log :unsigned-char i) 0))
    (setf (cffi:mem-ref length :int) 0)
    (ecase kind
      (:shader (gl-get-shader-info-log object 8192 length log))
      (:program (gl-get-program-info-log object 8192 length log)))
    (gl-check 'gl-object-log)
    (let ((count (cffi:mem-ref length :int)))
      (unless (<= 0 count 8191)
        (unavailable 'gl-object-log "Driver returned an invalid diagnostic byte count"))
      (cffi:foreign-string-to-lisp log :count count :encoding :utf-8))))

(defun compile-gl-shader (stage source)
  "Compile generated GLSL; transfer a checked nonzero shader to the caller."
  (gl-check 'before-compile-gl-shader)
  (call-with-gl-construction
   (lambda (own)
     (let* ((shader (gl-create-shader stage)) (owner (funcall own :shader shader)))
       (gl-check 'gl-create-shader)
       (unless owner (unavailable 'gl-create-shader "Driver returned shader zero"))
       (cffi:with-foreign-string (text source :encoding :utf-8)
         (cffi:with-foreign-object (strings :pointer)
           (setf (cffi:mem-ref strings :pointer) text)
           (gl-shader-source shader 1 strings (cffi:null-pointer))))
       (gl-check 'gl-shader-source)
       (gl-compile-shader shader)
       (gl-check 'gl-compile-shader)
       (when (zerop (gl-object-status :shader shader #x8B81))
         (unavailable 'compile-gl-shader (gl-object-log :shader shader)))
       owner))))

(defun link-gl-program (vertex-source fragment-source)
  "Compile/link generated text, checking temporary shader cleanup before transfer."
  (gl-check 'before-link-gl-program)
  (call-with-gl-construction
   (lambda (own)
     (let* ((vertex (compile-gl-shader #x8B31 vertex-source))
            (vertex-owner (funcall own :shader vertex))
            (fragment (compile-gl-shader #x8B30 fragment-source))
            (fragment-owner (funcall own :shader fragment))
            (program (gl-create-program))
            (owner (funcall own :program program)))
       (declare (ignore vertex-owner fragment-owner))
       (gl-check 'gl-create-program)
       (unless owner (unavailable 'gl-create-program "Driver returned program zero"))
       (gl-attach-shader program vertex)
       (gl-check 'gl-attach-vertex-shader)
       (gl-attach-shader program fragment)
       (gl-check 'gl-attach-fragment-shader)
       (gl-link-program program)
       (gl-check 'gl-link-program)
       (when (zerop (gl-object-status :program program #x8B82))
         (unavailable 'link-gl-program (gl-object-log :program program)))
       owner))))

(export '(gl-integer gl-integers gl-check compile-gl-shader link-gl-program
          gl-object-cleanup-error gl-cleanup-objects gl-cleanup-cause
          retry-gl-object-cleanup drain-gl-object-cleanup))
