;;;; Bounded lifecycle for the one newly launched direct child.
(in-package #:vttest-keyboard-driver)

(defparameter *keyboard-startup-seconds* 30)
(defparameter *keyboard-completion-seconds* 360)
(defparameter *keyboard-term-seconds* 15)
(defparameter *keyboard-kill-seconds* 5)
(defvar *keyboard-unjoined-children* nil)

(defun keyboard-deadline (seconds)
  (+ (get-internal-real-time) (* seconds internal-time-units-per-second)))
(defun keyboard-terminal-p (child)
  (member (sb-ext:process-status child) '(:exited :signaled)))
(defun keyboard-first-wait (child)
  ;; SBCL PROCESS-WAIT returns immediately for terminal status with no pipe
  ;; copiers. RUN-PROGRAM uses null stdin and two regular output files,
  ;; never :STREAM or a Lisp stream; no copier keeps its cookie nonzero.
  ;; Never call this on a running/stopped child, including from cleanup.
  (unless (keyboard-terminal-p child) (error "Gate is not terminal"))
  (sb-ext:process-wait child))

(defun keyboard-child-lifecycle (child root pid start)
  "Return title, normal exit code, actual first-wait flag, and diagnostics.
Only this handle is joined. A missing terminal observation retains the handle."
  (let ((title nil) (code nil) (waited nil) (errors nil) (terminal nil)
        (finish (keyboard-deadline *keyboard-completion-seconds*)))
    (labels ((note (e) (push (princ-to-string e) errors))
             (terminal-now ()
               (handler-case (setf terminal (keyboard-terminal-p child))
                 (error (e) (note e) nil)))
             (poll-until (deadline)
               (loop
                 (when (terminal-now) (return t))
                 (when (>= (get-internal-real-time) deadline) (return nil))
                 (sleep 0.02)))
             (signal-owned (signal)
               (handler-case
                   (if (and start (equal start (native-start pid)))
                       (multiple-value-bind (ok errno) (sb-ext:process-kill child signal :pid)
                         (unless ok (push (list :gate-signal-failed signal errno) errors)))
                       (push (list :gate-signal-identity-unverified signal) errors))
                 (error (e) (note e)))))
      ;; A title-read condition exits observation, but not the lifecycle.
      (handler-case
          (let ((startup (min finish (keyboard-deadline *keyboard-startup-seconds*))))
            (loop
              (when (terminal-now) (return))
              (setf title (keyboard-preclose-title root pid start))
              (when title (return))
              (when (>= (get-internal-real-time) startup)
                (push :gate-startup-timeout errors) (return))
              (sleep 0.02))
            (when (and title (not terminal) (not (poll-until finish)))
              (push :gate-completion-timeout errors)))
        (error (e) (note e)))
      ;; Give the gate time for its own owned cleanup before escalating.
      (unless (terminal-now)
        (signal-owned 15)
        (unless (poll-until (keyboard-deadline *keyboard-term-seconds*))
          (push :gate-term-grace-expired errors)
          (signal-owned 9)
          (unless (poll-until (keyboard-deadline *keyboard-kill-seconds*))
            (push :gate-terminal-unobserved errors))))
      (when terminal
        (handler-case
            (progn
              (keyboard-first-wait child)
              (setf waited t)
              (case (sb-ext:process-status child)
                (:exited (setf code (sb-ext:process-exit-code child)))
                (:signaled (push (format nil "Gate signaled: ~d" (sb-ext:process-exit-code child)) errors))))
          (error (e) (note e))))
      (if waited
          (handler-case (sb-ext:process-close child) (error (e) (note e)))
          (progn (push child *keyboard-unjoined-children*)
                 (push :gate-wait-unobserved errors)))
      (values title code waited (nreverse errors)))))
