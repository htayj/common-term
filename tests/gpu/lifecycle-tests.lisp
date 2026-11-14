(in-package #:lispterm.gpu.tests)

(defmacro with-test-function ((name function) &body body)
  `(let ((saved (symbol-function ',name)))
     (unwind-protect (progn (setf (symbol-function ',name) ,function) ,@body)
       (setf (symbol-function ',name) saved))))

(defun check-display-owner-rejection (display)
  (let ((thread (sb-thread:make-thread
                 (lambda ()
                   (handler-case (progn (lispterm.display:display-framebuffer display) nil)
                     (lispterm.display:display-error () t))))))
    (check (sb-thread:join-thread thread) "Foreign thread accessed native display"))
  (let ((pid (lispterm.display::display-process-id display)))
    (unwind-protect
         (progn (setf (lispterm.display::display-process-id display) (1+ pid))
                (check (handler-case (progn (lispterm.display:present display) nil)
                         (lispterm.display:display-error () t)) "Inherited process accessed native display"))
      (setf (lispterm.display::display-process-id display) pid))))

(defun check-initialization-policy (backend name)
  (with-test-function (lispterm.gpu:open-context
                       (lambda (&rest args) (declare (ignore args))
                         (lispterm.gpu:unavailable 'test-initialize "controlled unavailable device")))
    (let ((display (lispterm.display:open-display :backend backend :display-name name :renderer :auto :columns 3 :rows 2)))
      (unwind-protect
           (check (and (eq :cpu (lispterm.display::display-renderer-kind display))
                       (lispterm.display::display-renderer-fallback-reason display)) "Auto failed clean unavailable fallback")
        (lispterm.display:close-display display)))
    (check (handler-case
               (progn (lispterm.display:open-display :backend backend :display-name name :renderer :gpu :columns 3 :rows 2) nil)
             (lispterm.display:display-open-failed () t)) "Explicit GPU silently fell back"))
  (let ((context (lispterm.gpu:open-context)))
    (with-test-function (lispterm.gpu:open-context
                         (lambda (&rest args) (declare (ignore args))
                           (error 'lispterm.gpu:gpu-cleanup-error :operation 'test-initialize
                                  :detail "controlled retained native state" :context context)))
      (check (handler-case
                 (progn (lispterm.display:open-display :backend backend :display-name name :renderer :auto :columns 3 :rows 2) nil)
               (lispterm.display:display-open-failed (condition)
                 (check (typep (lispterm.display:display-error-detail condition) 'lispterm.gpu:gpu-cleanup-error)
                        "Retained-state initialization condition was lost")
                 t)) "Auto swallowed GPU cleanup error"))
    (check (lispterm.gpu::context-closed-p context) "Retained initialization context was leaked"))
  t)

(defun display-lifecycle-check (backend name)
  (let ((display (lispterm.display:open-display :backend backend :display-name name :renderer :gpu :columns 12 :rows 4)))
    (unwind-protect
         (progn
           (native-parity-frame display)
           (check-display-owner-rejection display)
           (check (handler-case (progn (lispterm.gpu::require-no-live-contexts) nil)
                    (lispterm.gpu:gpu-unavailable () t)) "Checkpoint accepted live GPU context")
           (let ((epoch (lispterm.display::display-epoch display))
                 (configuration (copy-list (lispterm.display::display-configuration display))))
             (setf display (lispterm.display:reopen-display display))
             (check (> (lispterm.display::display-epoch display) epoch) "Reopen reused display generation")
             (check (equal configuration (lispterm.display::display-configuration display)) "Reopen lost renderer configuration"))
           (native-parity-frame display)
           (lispterm.display:resize-display display 105 69)
           (check (null (lispterm.display::display-framebuffer-storage display)) "Resize allocated CPU composition frame")
           (native-parity-frame display)
           (let ((calls 0) (original (symbol-function 'lispterm.display::render-cell)))
             (with-test-function (lispterm.display::render-cell
                                  (lambda (&rest args) (incf calls) (apply original args)))
               (lispterm.display::render-cell display 0 0 (lispterm.display::make-cell :char #\Z)))
             (check (= 1 calls) "Live neutral cell redefinition bypassed")
             (lispterm.display:present display))
           (let ((context (lispterm.display::display-renderer-context display)))
             (with-test-function (lispterm.gpu:close-context
                                  (lambda (context) (declare (ignore context))
                                    (lispterm.gpu:unavailable 'test-close "injected cleanup failure")))
               (check (handler-case (progn (lispterm.display:close-display display) nil)
                        (lispterm.gpu:gpu-unavailable () t)) "Cleanup failure did not propagate")
               (check (and (lispterm.display:display-cleanup-pending-p display)
                           (eq context (lispterm.display::display-renderer-context display))) "Failed cleanup lost recovery handle"))
             (lispterm.display:close-display display)
             (check (and (lispterm.gpu::context-closed-p context)
                         (not (lispterm.display:display-cleanup-pending-p display))) "Cleanup retry failed")))
      (lispterm.display:close-display display)))
  (check-initialization-policy backend name)
  (lispterm.gpu::require-no-live-contexts)
  (format t "LIFECYCLE ~A resize/reopen/redefine/owner/save-guard/cleanup-retry/initialization-policy PASS~%" backend)
  t)

(defun gpu-lifecycle-check ()
  (dolist (backend '(:x11 :wayland))
    (call-private-display backend (lambda (name) (display-lifecycle-check backend name))))
  t)

(register-check "gpu-lifecycle" #'gpu-lifecycle-check)
