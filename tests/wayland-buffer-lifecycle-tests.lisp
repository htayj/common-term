;;;; Deterministic replay of the real desktop's release-after-local-destroy race.
(in-package #:lispterm.display.wayland)

(defmacro with-buffer-test-functions (bindings &body body)
  "Replace external effects only for BODY, restoring even after test failure."
  (let ((saved (loop for binding in bindings collect (gensym "ORIGINAL"))))
    `(let ,(loop for (name replacement) in bindings for old in saved
                 collect `(,old (symbol-function ',name)))
       (unwind-protect
            (progn
              ,@(loop for (name replacement) in bindings
                      collect `(setf (symbol-function ',name) ,replacement))
              ,@body)
         ,@(loop for (name replacement) in bindings for old in saved
                 collect `(setf (symbol-function ',name) ,old))))))

(defun buffer-test-frame (id opcode &optional (body #()))
  (concatenate '(vector (unsigned-byte 8)) (u32-bytes id)
               (u32-bytes (logior (ash (+ 8 (length body)) 16) opcode)) body))

(defun buffer-test-dispatch (wire bytes)
  (setf (wire-bytes wire) bytes)
  (wire-dispatch wire))

(defun buffer-test-rejected (wire bytes &optional operation)
  (let ((condition (handler-case (progn (buffer-test-dispatch wire bytes) nil)
                     (wayland-transport-error (c) c))))
    (assert condition)
    (when operation (assert (equal operation (transport-operation condition))))
    ;; A rejected message remains in the input. Each probe starts independently.
    (setf (wire-bytes wire) (make-array 0 :element-type '(unsigned-byte 8)))))

(defun call-with-buffer-test-state (function)
  "Run the real protocol-open setup with a socket-free server handshake.
The production display handler, buffer creation and wire dispatcher are unchanged."
  (let ((wire (%make-wire)) (requests nil) (unmaps nil)
        (real-unmap (symbol-function '%munmap)) (state nil))
    (with-buffer-test-functions
        ((wire-connect (lambda (&optional name) (declare (ignore name)) wire))
         (wire-send (lambda (w id opcode &rest args)
                      (assert (eq w wire)) (push (list* id opcode args) requests)))
         (%munmap (lambda (pointer size)
                    (push (list pointer size) unmaps)
                    (funcall real-unmap pointer size)))
         (protocol-roundtrip
          (lambda (s &optional timeout)
            (declare (ignore timeout))
            (unless (wayland-state-compositor s)
              (bind-global s 100 "wl_compositor" 4)
              (bind-global s 101 "wl_shm" 1)
              (bind-global s 102 "xdg_wm_base" 1))))
         (protocol-pump (lambda (s &optional timeout)
                          (declare (ignore timeout))
                          (setf (wayland-state-configured-p s) t))))
      (unwind-protect
           (progn
             (setf state (protocol-open))
             ;; The desktop's pool/buffer allocation used IDs 15 and 16.
             (setf (wire-next-id wire) 15)
             (let ((buffer (make-protocol-buffer state 2 2)))
               (assert (= 16 (shm-buffer-id buffer)))
               (setf requests nil unmaps nil)
               (funcall function state buffer
                        (lambda () requests) (lambda () unmaps))))
        (when state (protocol-close state))))))

(defun buffer-test-destroy-count (requests)
  (count '(16 0) requests :test #'equal))

(defun buffer-test-retirement-replay (concatenated)
  (call-with-buffer-test-state
   (lambda (state buffer requests unmaps)
     (let* ((wire (wayland-state-wire state))
            ;; Exact received header from launch65, not merely a decoded mock.
            (release (make-array 8 :element-type '(unsigned-byte 8)
                                  :initial-contents '(16 0 0 0 0 0 8 0)))
            (delete-id (buffer-test-frame 1 1 (u32-bytes 16))))
       (setf (wayland-state-generation state) 1)
       (protocol-collect-buffers state)
       ;; Dispatch first so baseline failure is the observed unknown-object16,
       ;; rather than an assertion about new state or test infrastructure.
       (handler-case
           (if concatenated
               (buffer-test-dispatch wire
                                    (concatenate '(vector (unsigned-byte 8)) release delete-id))
               (buffer-test-dispatch wire release))
         (wayland-transport-error (c)
           (format *error-output* "BUFFER-REPLAY-FAILURE: ~A~%" (transport-operation c))
           (error c)))
       (assert (null (wayland-state-buffers state)))
       (assert (null (shm-buffer-pointer buffer)))
       (assert (= 1 (buffer-test-destroy-count (funcall requests))))
       (assert (= 1 (length (funcall unmaps))))
       (unless concatenated
         (assert (gethash 16 (wire-handlers wire)))
         (assert (eq :buffer (gethash 16 (wire-interfaces wire))))
         (assert (not (shm-buffer-busy-p buffer)))
         (buffer-test-dispatch wire delete-id))
       (assert (not (gethash 16 (wire-handlers wire))))
       (assert (not (gethash 16 (wire-interfaces wire))))
       (buffer-test-rejected wire release "unknown object 16")
       (buffer-test-rejected wire (buffer-test-frame 999 0) "unknown object 999")))))

(defun buffer-test-release-validation ()
  (call-with-buffer-test-state
   (lambda (state buffer requests unmaps)
     (declare (ignore requests unmaps))
     (let ((wire (wayland-state-wire state)))
       (dolist (retired '(nil t))
         (when retired (destroy-shm-buffer state buffer))
         (setf (shm-buffer-busy-p buffer) t)
         (buffer-test-dispatch wire (buffer-test-frame 16 0))
         (assert (not (shm-buffer-busy-p buffer)))
         (buffer-test-rejected wire (buffer-test-frame 16 1)
                               "invalid wl_buffer.release event")
         (buffer-test-rejected wire (buffer-test-frame 16 0 (u32-bytes 0))
                               "invalid wl_buffer.release event")
         ;; The wire parser, rather than a permissive draining handler, rejects
         ;; both a short header size and an unaligned declared message size.
         (dolist (size '(4 9))
           (buffer-test-rejected
            wire (concatenate '(vector (unsigned-byte 8)) (u32-bytes 16)
                              (u32-bytes (ash size 16)) (u32-bytes 0)))))))))

(defun buffer-test-cleanup (connected-first)
  (call-with-buffer-test-state
   (lambda (state buffer requests unmaps)
     (let ((wire (wayland-state-wire state)))
       (when connected-first
         (destroy-shm-buffer state buffer)
         (destroy-shm-buffer state buffer)
         (assert (gethash 16 (wire-handlers wire))))
       (destroy-shm-buffer state buffer t)
       (destroy-shm-buffer state buffer t)
       ;; Re-entering connected cleanup must not send after disconnected cleanup.
       (destroy-shm-buffer state buffer)
       (assert (= (if connected-first 1 0)
                  (buffer-test-destroy-count (funcall requests))))
       (assert (= 1 (length (funcall unmaps))))
       (assert (null (shm-buffer-pointer buffer)))
       (assert (not (gethash 16 (wire-handlers wire))))))))

(defun buffer-test-unmapped-destruction ()
  (call-with-buffer-test-state
   (lambda (state buffer requests unmaps)
     (declare (ignore buffer unmaps))
     ;; A missing mapping is not evidence that a destroy request was sent.
     (let ((unmapped (make-shm-buffer :id 99 :pointer nil)))
       (destroy-shm-buffer state unmapped)
       (destroy-shm-buffer state unmapped)
       (assert (= 1 (count '(99 0) (funcall requests) :test #'equal)))))))

(buffer-test-retirement-replay nil)
(buffer-test-retirement-replay t)
(format t "BUFFER-LIFECYCLE-REPLAY-PASS: separate and concatenated release/delete_id~%")
(unless (uiop:getenv "LISPTERM_BUFFER_REPLAY_ONLY")
  (buffer-test-release-validation)
  (buffer-test-cleanup nil)
  (buffer-test-cleanup t)
  (buffer-test-unmapped-destruction)
  (format t "BUFFER-LIFECYCLE-PASS: validation and idempotent cleanup~%"))
