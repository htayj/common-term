;;;; Every LTD call happens on the window owner, including open and close.
(in-package #:lispterm)

(defstruct (host (:constructor %make-host))
  id session configuration thread display effective-backend
  (lock (sb-thread:make-mutex :name "lispterm host"))
  (control-lock (sb-thread:make-mutex :name "lispterm lifecycle"))
  (state :detached) commands (error-ring nil) (generation 0) (stop-p nil))
(defstruct host-command operation argument (state :queued) result error
  (done (sb-thread:make-semaphore :name "lispterm command")))
(defstruct lifecycle-token host generation attached-p (used-p nil))

(defun host-record-error (host operation condition)
  (let* ((text (princ-to-string condition))
         (entry (list :time (get-universal-time) :operation operation
                      :message (subseq text 0 (min 512 (length text))))))
    (sb-thread:with-mutex ((host-lock host))
      (setf (host-error-ring host) (subseq (cons entry (host-error-ring host))
                                          0 (min 32 (1+ (length (host-error-ring host)))))))))

(defun host-errors (host)
  (sb-thread:with-mutex ((host-lock host))
    (mapcar (lambda (entry)
              (list :time (getf entry :time) :operation (getf entry :operation)
                    :message (copy-seq (getf entry :message))))
            (host-error-ring host))))

(defun host-state-value (host)
  (sb-thread:with-mutex ((host-lock host)) (host-state host)))
(defun set-host-state (host state)
  (sb-thread:with-mutex ((host-lock host)) (setf (host-state host) state)))

(defun window-close (host)
  ;; Retain the handle on close failure, so checkpoint quiesce cannot silently
  ;; claim that an unclosed native resource is safe to inherit across fork.
  (when (host-display host)
    (handler-case (ltd:close-display (host-display host))
      (error (condition)
        (set-host-state host :detached)
        (error condition)))
    (setf (host-display host) nil))
  (set-host-state host :detached))

(defun window-open (host)
  (window-close host)
  (let* ((session (host-session host))
         (screen (session-screen session)))
    (handler-case
        (setf (host-display host)
              (apply #'ltd:open-display :rows (lispterm.engine:rows screen)
                     :columns (lispterm.engine:columns screen) (host-configuration host)))
      (ltd:display-open-failed (condition)
        (let ((display (ltd:display-open-failed-display condition)))
          (when (and display (ltd:display-cleanup-pending-p display))
            (setf (host-display host) display)))
        (set-host-state host :detached)
        (error condition)))
    (sb-thread:with-mutex ((session-lock session))
      (let ((title (nerimux/terminal:screen-title screen)))
        (when (and title (plusp (length title)))
          (ltd:set-title (host-display host) (subseq title 0 (min 1024 (length title)))))
        (setf (session-force-frame session) t
              (session-last-title session) (and title (copy-seq title)))))
    (sb-thread:with-mutex ((host-lock host))
      (setf (host-effective-backend host) (ltd:display-backend (host-display host))
            (host-state host) :attached))))

(defun display-step (host)
  (let ((display (host-display host)))
    (when (and display (eq (host-state-value host) :attached))
      (restart-case
          (progn
            (dolist (event (ltd:poll-events display :timeout 0))
              (if (typep event 'ltd:close-event) (window-close host)
                  (session-event (host-session host) event)))
            (when (eq (host-state-value host) :attached)
              (render-session (host-session host) display)))
        (reopen () :report "Replace the display and keep the PTY session." (window-open host))
        (detach () :report "Close the window, retaining the terminal session." (window-close host))
        (abort () :report "Stop this display iteration." nil)))))

(defun dispatch-host-command (host command)
  (case (host-command-operation command)
    (:reopen (window-open host))
    (:detach (window-close host))
    (:input (session-write (host-session host) (host-command-argument command)))
    (:event (session-event (host-session host) (host-command-argument command)))
    (:invalidate
     (sb-thread:with-mutex ((session-lock (host-session host)))
       (setf (session-force-frame (host-session host)) t)))
    (:stop
     ;; A timed-out quiesce must not later stop its owner without a token.
     ;; Commit the stop under the same lock as caller-side cancellation.
     (let ((attached-p (eq (host-state-value host) :attached)) cancelled-p)
       (window-close host)
       (sb-thread:with-mutex ((host-lock host))
         (if (eq (host-command-state command) :cancelled)
             (setf cancelled-p t)
             (setf (host-command-state command) :committed (host-stop-p host) t)))
       (when (and cancelled-p attached-p) (window-open host))))
    (otherwise (error 'host-error :operation :command :detail "Unknown operation")))
  host)

(defun run-host-commands (host)
  (let ((commands (sb-thread:with-mutex ((host-lock host))
                    (prog1 (host-commands host) (setf (host-commands host) nil)))))
    (dolist (command commands)
      (when (sb-thread:with-mutex ((host-lock host))
              (when (eq (host-command-state command) :queued)
                (setf (host-command-state command) :running)))
        (handler-case
            (setf (host-command-result command) (dispatch-host-command host command))
          (error (condition)
            (setf (host-command-error command) condition)
            (host-record-error host (host-command-operation command) condition)))
        (sb-thread:with-mutex ((host-lock host)) (setf (host-command-state command) :done))
        (sb-thread:signal-semaphore (host-command-done command))))))

(defun window-worker (host)
  (let ((next-frame 0) (io-backoff 0))
    (unwind-protect
         (loop until (host-stop-p host) do
           (run-host-commands host)
           (unless (host-stop-p host)
             (let ((now (get-internal-real-time)))
               (when (>= now io-backoff)
                 (handler-case
                     (when (eq (session-read-output (host-session host)) :eof)
                       (setf io-backoff (+ now (* 1/10 internal-time-units-per-second))))
                   (error (condition)
                     (host-record-error host :pty condition)
                     (setf io-backoff (+ now internal-time-units-per-second)))))
               (when (>= now next-frame)
                 (setf next-frame (+ now (ceiling internal-time-units-per-second 60)))
                 (handler-case (display-step host)
                   (error (condition)
                     (host-record-error host :display condition)
                     (set-host-state host :detached)
                     (handler-case (window-close host)
                       (error (close-error) (host-record-error host :display-close close-error)))))))
             ;; Bound idle CPU use, even when poll returns immediately on EOF.
             (sleep 0.005)))
      (handler-case (window-close host)
        (error (condition) (host-record-error host :worker-cleanup condition)))
      (set-host-state host :quiesced))))

(defun start-window-worker (host)
  (sb-thread:with-mutex ((host-lock host))
    (setf (host-stop-p host) nil
          (host-thread host) (sb-thread:make-thread (lambda () (window-worker host))
                                                  :name "lispterm session/display"))))

(defun call-host (host operation &optional argument (timeout 5))
  (let ((command (make-host-command :operation operation :argument argument)))
    (sb-thread:with-mutex ((host-lock host))
      (unless (and (host-thread host) (sb-thread:thread-alive-p (host-thread host)))
        (error 'host-error :operation operation :detail "Host has no running owner"))
      (when (>= (length (host-commands host)) 128)
        (error 'host-error :operation operation :detail "Host command queue is full"))
      (setf (host-commands host) (nconc (host-commands host) (list command))))
    (unless (sb-thread:wait-on-semaphore (host-command-done command) :timeout timeout)
      (unless (sb-thread:with-mutex ((host-lock host))
                (if (member (host-command-state command) '(:done :committed)) t
                    (progn (setf (host-command-state command) :cancelled) nil)))
        (error 'host-timeout :operation operation :detail "Owner did not finish within the bounded wait")))
    (when (host-command-error command) (error (host-command-error command)))
    (host-command-result command)))
