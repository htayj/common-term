;;;; Every LTD call happens on the window owner, including open and close.
(in-package #:lispterm)

(defstruct (host (:constructor %make-host))
  id session configuration thread display effective-backend
  geometry-sent-generation
  (lock (sb-thread:make-mutex :name "lispterm host"))
  (control-lock (sb-thread:make-mutex :name "lispterm lifecycle"))
  (state :detached) commands (error-ring nil) (generation 0) (stop-p nil)
  (cleanup-pending-p nil))
(defstruct host-command operation argument (state :queued) result error
  (done (sb-thread:make-semaphore :name "lispterm command")))
(defstruct lifecycle-token host generation attached-p (used-p nil))

(define-condition host-cleanup-pending (host-error) ()
  (:documentation "A retained native display must close before the host accepts other work."))
(export 'host-cleanup-pending)

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

(defun window-release-x11-grid (session)
  (sb-thread:with-mutex ((session-lock session))
    (when (session-x11-grid session)
      (session-restore-x11-grid session)
      (setf (session-x11-grid session) nil
            (session-geometry-intent session) nil
            (session-geometry-paused-p session) nil))))

(defun window-set-cell-metrics (session display)
  ;; Caller holds SESSION-LOCK. X11 cells are already in pixels. On Wayland
  ;; the font describes logical cells and the whole frame is scaled to the buffer.
  (let* ((font (ltd:display-font display))
         (backend (ltd:display-backend display))
         (scale (or (when (and font (eq backend :wayland))
                      (getf (ltd:backend-metrics backend display) :buffer-scale))
                    1)))
    (setf (session-cell-width session) (and font (* (ltd:font-width font) scale))
          (session-cell-height session) (and font (* (ltd:font-height font) scale)))))

(defun window-close (host)
  ;; Retain the handle on close failure, so checkpoint quiesce cannot silently
  ;; claim that an unclosed native resource is safe to inherit across fork.
  (when (host-display host)
    (window-release-x11-grid (host-session host))
    (let ((session (host-session host)))
      (sb-thread:with-mutex ((session-lock session))
        (setf (session-cell-width session) nil (session-cell-height session) nil)))
    (handler-case (ltd:close-display (host-display host))
      (error (condition)
        (set-host-state host :detached)
        (error condition)))
    (let ((session (host-session host)))
      (sb-thread:with-mutex ((session-lock session))
        (setf (session-geometry-status session)
              (ltd:geometry-status (host-display host)))))
    (setf (host-display host) nil (host-geometry-sent-generation host) nil))
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
    (let* ((display (host-display host))
           (x11-p (ltd:configure-authoritative-p display)))
      (sb-thread:with-mutex ((session-lock session))
        (window-set-cell-metrics session display))
      ;; Install the text-area size before STARTUP can exec the child.
      (session-resize session (ltd:display-rows display) (ltd:display-columns display)
                      :x11-configure-p x11-p))
    (sb-thread:with-mutex ((session-lock session))
      (let ((title (nerimux/terminal:screen-title screen)))
        (when (and title (plusp (length title)))
          (ltd:set-title (host-display host) (subseq title 0 (min 1024 (length title)))))
        (setf (session-force-frame session) t
              (session-last-title session) (and title (copy-seq title)))))
    (sb-thread:with-mutex ((host-lock host))
      (setf (host-effective-backend host) (ltd:display-backend (host-display host))
            (host-state host) :attached))))

(defun display-step (host &key (poll-p t) (render-p t))
  "Poll independently of rendering. Reject saturated native input one whole event at a time."
  (let ((display (host-display host)))
    (when (and display (eq (host-state-value host) :attached))
      (restart-case
          (let ((session (host-session host)))
            (when poll-p
              (lispterm.trace:trace-event :display-step)
              (window-geometry-intent host display)
              (let ((events (ltd:poll-events display :timeout 0)))
                ;; Poll already applied native geometry. Read metrics before
                ;; publishing a grid so all four fields change together.
                (sb-thread:with-mutex ((session-lock session))
                  (window-set-cell-metrics session display))
                (dolist (event events)
                  (lispterm.trace:trace-event :event-dequeue :kind (type-of event))
                  (cond ((typep event 'ltd:close-event) (window-close host))
                        ((typep event 'ltd:resize-event)
                         (window-geometry-event host display event))
                        (t
                         (handler-case (session-event session event)
                           (input-backpressure (condition)
                             ;; Never replay rejected input.
                             (host-record-error host :input-backpressure condition)
                             (lispterm.trace:trace-event :input-backpressure
                               :kind (type-of event)))))))))
            (when (eq (host-state-value host) :attached)
              (when poll-p
                ;; Font or output scale can change without a resize event.
                (sb-thread:with-mutex ((session-lock session))
                  (session-update-current-winsize session)))
              (if (session-x11-grid session)
                  (progn
                    (when poll-p
                      (ltd:geometry-tick display)
                      (window-geometry-commit host display))
                    ;; Draw only when the session fits the actual X11 window.
                    (let ((grid (session-x11-grid session)))
                      (when (and render-p (session-screen-size-p session (car grid) (cdr grid)))
                        (render-session session display))))
                  (when render-p
                    (ltd:geometry-tick display)
                    (render-session session display)))
              (sb-thread:with-mutex ((session-lock session))
                (setf (session-geometry-status session)
                      (append (ltd:geometry-status display) (ltd:display-metrics display))))))
        (reopen () :report "Replace the display and keep the PTY session." (window-open host))
        (detach () :report "Close the window, retaining the terminal session." (window-close host))
        (abort () :report "Stop this display iteration." nil)))))

(defun dispatch-host-command (host command)
  (case (host-command-operation command)
    (:startup
     (window-open host)
     ;; Claim the irreversible spawn under the cancellation lock, but never
     ;; hold that lock over PTY operations. A timeout after this claim still
     ;; cancels startup: OPEN must join this owner before releasing its PTY.
     (when (sb-thread:with-mutex ((host-lock host))
             (unless (eq (host-command-state command) :cancelled)
               (setf (host-command-state command) :spawning)))
       (when (host-command-argument command)
         (lispterm.pty:spawn-on-pty (session-pty (host-session host))
                                    (host-command-argument command)))))
    (:reopen (window-open host))
    (:detach (window-close host))
    (:input (session-write (host-session host) (host-command-argument command)))
    (:event (session-event (host-session host) (host-command-argument command)))
    (:geometry-retry
     (let* ((session (host-session host)) (display (host-display host)))
       (sb-thread:with-mutex ((session-lock session))
         (destructuring-bind (generation rows columns) (session-display-intent session)
           (ltd:request-grid-size display columns rows generation)
           (setf (host-geometry-sent-generation host) generation))
         (setf (session-force-frame session) t))))
    (:geometry-cancel
     (let ((display (host-display host)))
       (ltd:cancel-size-request display)
       (let ((session (host-session host)))
         (if (session-x11-grid session)
             (sb-thread:with-mutex ((session-lock session))
               (session-restore-x11-grid session)
               (setf (session-geometry-paused-p session) nil
                     (host-geometry-sent-generation host) (session-geometry-generation session)))
             (session-resize session (ltd:display-rows display) (ltd:display-columns display))))))
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

(defun run-host-command (host command)
  "Execute one claimed command; return true only for uncancelled success."
  (when (sb-thread:with-mutex ((host-lock host))
          (when (eq (host-command-state command) :queued)
            (setf (host-command-state command) :running)))
    (handler-case
        (setf (host-command-result command) (dispatch-host-command host command))
      (error (condition)
        (setf (host-command-error command) condition)
        (host-record-error host (host-command-operation command) condition)))
    (let ((success
            (sb-thread:with-mutex ((host-lock host))
              (unless (eq (host-command-state command) :cancelled)
                (setf (host-command-state command) :done)
                (null (host-command-error command))))))
      (sb-thread:signal-semaphore (host-command-done command))
      success)))

(defun run-host-commands (host)
  (let ((commands (sb-thread:with-mutex ((host-lock host))
                    (prog1 (host-commands host) (setf (host-commands host) nil)))))
    (dolist (command commands) (run-host-command host command))))

(defun reject-host-command (host command)
  "Complete COMMAND with HOST-CLEANUP-PENDING without executing it."
  (let ((condition (make-condition 'host-cleanup-pending
                                   :operation (host-command-operation command) :host host
                                   :detail "Native display cleanup is pending")))
    (host-record-error host (host-command-operation command) condition)
    (sb-thread:with-mutex ((host-lock host))
      (setf (host-command-error command) condition)
      (unless (eq (host-command-state command) :cancelled)
        (setf (host-command-state command) :done)))
    (sb-thread:signal-semaphore (host-command-done command))))

(defun supervise-display-cleanup (host)
  "Own a display retained by failed startup until a queued :STOP closes it.
Other commands are rejected; the PTY session and renderer are never touched."
  (sb-thread:with-mutex ((host-lock host)) (setf (host-cleanup-pending-p host) t))
  (loop while (host-display host) do
    (dolist (command (sb-thread:with-mutex ((host-lock host))
                       (prog1 (host-commands host) (setf (host-commands host) nil))))
      (if (eq (host-command-operation command) :stop)
          (run-host-command host command)
          (reject-host-command host command)))
    (sleep 0.005)))

(defparameter *owner-clock* #'get-internal-real-time)
(defparameter *owner-wait* (lambda (host seconds) (declare (ignore host)) (sleep seconds)))

(defstruct owner-schedule (next-frame 0) (io-backoff 0))

(defun owner-iteration (host schedule)
  "Bound input draining and output reading; poll independently of draw pacing."
  (when (host-stop-p host) (return-from owner-iteration nil))
  (run-host-commands host)
  (when (host-stop-p host) (return-from owner-iteration nil))
  (let ((session (host-session host)) (output-p nil) (input-status :empty))
    (labels ((display-failure (condition)
               (host-record-error host :display condition)
               (set-host-state host :detached)
               (handler-case (window-close host)
                 (error (close-error) (host-record-error host :display-close close-error))))
             (drain-input ()
               (multiple-value-bind (count status)
                   (session-flush-input session :max-writes 16 :max-bytes 16384)
                 (declare (ignore count))
                 (setf input-status status))
               (session-queue-responses session))
             (input-failure (condition)
               (host-record-error host :pty condition)
               (setf (owner-schedule-io-backoff schedule)
                     (+ (funcall *owner-clock*) internal-time-units-per-second))))
      ;; Clear old bytes and response backlog BEFORE admitting newly polled keys.
      (handler-case (drain-input) (error (condition) (input-failure condition)))
      (handler-case
          (display-step host :render-p nil)
        (error (condition) (display-failure condition)))
      (when (host-stop-p host) (return-from owner-iteration nil))
      ;; Write every available event in a bounded batch, even during read backoff.
      (handler-case (drain-input) (error (condition) (input-failure condition)))
      (let ((now (funcall *owner-clock*)))
        (when (>= now (owner-schedule-io-backoff schedule))
          (handler-case
              (let* ((pending (session-input-pending session))
                     (result (session-read-output session)))
                ;; The read's final default flush may become writable after the
                ;; earlier drain blocked. Do not sleep on that positive suffix.
                (when (< (session-input-pending session) pending)
                  (setf input-status (if (plusp (session-input-pending session))
                                         :budget :empty)))
                (cond ((eq result :eof)
                       (setf (owner-schedule-io-backoff schedule)
                             (+ now (* 1/10 internal-time-units-per-second))))
                      ((and (vectorp result) (plusp (length result)))
                       (setf output-p t))))
            (error (condition) (input-failure condition)))))
      (let ((now (funcall *owner-clock*)))
        (when (>= now (owner-schedule-next-frame schedule))
          (let ((frames (session-frames session)))
            (handler-case (display-step host :poll-p nil)
              (error (condition) (display-failure condition)))
            (when (/= frames (session-frames session))
              (let* ((period (/ internal-time-units-per-second 60))
                     (deadline (owner-schedule-next-frame schedule)))
                (setf (owner-schedule-next-frame schedule)
                      (if (> (- now deadline) period)
                          (+ now period)
                          (+ deadline (* period (1+ (floor (- now deadline) period)))))))))))
    ;; A budget-limited writable FIFO gets another fair turn immediately. A zero
    ;; write must wait, rather than spin on non-writable PTY input. Pending output
    ;; still gets its normal bounded read on each turn.
    (if (or output-p (eq input-status :budget)) 0
        (let* ((now (funcall *owner-clock*))
               (deadline (owner-schedule-next-frame schedule))
               (blink (session-next-blink session))
               (backoff (owner-schedule-io-backoff schedule))
               (wait 0.005))
          (dolist (target (list deadline (and blink (* blink internal-time-units-per-second)) backoff))
            (when (and target (> target now))
              (setf wait (min wait (/ (- target now) internal-time-units-per-second)))))
          wait)))))

(defun window-worker (host &optional startup-command)
  (let ((schedule (make-owner-schedule)))
    (unwind-protect
         (progn
           (when (and startup-command (not (run-host-command host startup-command)))
             ;; Cancelled startup has no caller left to send :STOP, so try the
             ;; close here. A completed failure is stopped by its caller.
             (when (sb-thread:with-mutex ((host-lock host))
                     (eq (host-command-state startup-command) :cancelled))
               (handler-case (window-close host)
                 (error (condition) (host-record-error host :worker-cleanup condition))))
             (when (host-display host)
               (supervise-display-cleanup host))
             (return-from window-worker nil))
           (loop for wait = (owner-iteration host schedule)
                 while wait do (funcall *owner-wait* host wait)))
      (handler-case (window-close host)
        (error (condition) (host-record-error host :worker-cleanup condition)))
      (set-host-state host :quiesced))))

(defun start-window-worker (host &optional startup-command)
  (sb-thread:with-mutex ((host-lock host))
    (setf (host-stop-p host) nil
          (host-cleanup-pending-p host) nil
          (host-thread host) (sb-thread:make-thread
                              (lambda () (window-worker host startup-command))
                              :name "lispterm session/display"))))

(defun wait-host-command (host command timeout)
  (unless (sb-thread:wait-on-semaphore (host-command-done command) :timeout timeout)
    (unless (sb-thread:with-mutex ((host-lock host))
              (if (member (host-command-state command) '(:done :committed)) t
                  (progn (setf (host-command-state command) :cancelled) nil)))
      (error 'host-timeout :operation (host-command-operation command)
             :detail "Owner did not finish within the bounded wait")))
  (when (host-command-error command) (error (host-command-error command)))
  (host-command-result command))

(defun call-host (host operation &optional argument (timeout 5))
  (let ((command (make-host-command :operation operation :argument argument)))
    (sb-thread:with-mutex ((host-lock host))
      (unless (and (host-thread host) (sb-thread:thread-alive-p (host-thread host)))
        (error 'host-error :operation operation :detail "Host has no running owner"))
      (when (>= (length (host-commands host)) 128)
        (error 'host-error :operation operation :detail "Host command queue is full"))
      (setf (host-commands host) (nconc (host-commands host) (list command))))
    (wait-host-command host command timeout)))

(defun session-display-intent (session)
  "Return generation, rows and columns under SESSION-LOCK."
  (or (and (session-x11-grid session) (session-geometry-intent session))
      (list (session-geometry-generation session)
            (lispterm.engine:rows (session-screen session))
            (lispterm.engine:columns (session-screen session)))))

(defun window-geometry-intent (host display)
  (let* ((session (host-session host))
         (intent (sb-thread:with-mutex ((session-lock session))
                   (session-display-intent session))))
    (unless (eql (first intent) (host-geometry-sent-generation host))
      (destructuring-bind (generation rows columns) intent
        ;; A fresh native open already requested the initial dimensions.
        (unless (and (zerop generation)
                     (= columns (ltd:display-columns display)) (= rows (ltd:display-rows display)))
          (ltd:request-grid-size display columns rows generation))
        (setf (host-geometry-sent-generation host) generation)))))

(defun window-geometry-commit (host display)
  "Release parsing only after the matched request resolves and the actual grid is published."
  (let ((session (host-session host)) (request (ltd:display-geometry-request display)))
    (when (and (session-geometry-paused-p session) request
               (member (ltd:geometry-request-phase request)
                       '(:surface-ready :closing :settled :refused :timed-out)))
      (sb-thread:with-mutex ((session-lock session))
        (let ((grid (session-x11-grid session)))
          ;; Poll has already processed the coalesced actual size event, which
          ;; records GRID only after the PTY resize succeeds. Same-size requests
          ;; may reach :SURFACE-READY without a size event, via GetGeometry.
          (when (eql (ltd:geometry-request-id request) (session-geometry-generation session))
            (multiple-value-bind (x y) (session-winsize-pixels session (car grid) (cdr grid))
              (unless (and (eql (car grid) (ltd:display-rows display))
                           (eql (car grid) (session-pty-rows session))
                           (eql (cdr grid) (ltd:display-columns display))
                           (eql (cdr grid) (session-pty-columns session))
                           (eql x (session-pty-x-pixels session))
                           (eql y (session-pty-y-pixels session))
                           (session-screen-size-p session (car grid) (cdr grid)))
                ;; Detach through the owner's display error path rather than
                ;; stall or resume with different engine and PTY sizes.
                (error 'host-error :operation :resize :detail "X11 and PTY winsizes disagree")))
            (setf (session-geometry-paused-p session) nil)))))))

(defun window-geometry-event (host display event)
  (let ((session (host-session host)))
    (when (session-x11-grid session)
      ;; X11 poll already applied every actual size to the display. When it
      ;; drained several configures, publish only the newest actual size.
      (return-from window-geometry-event
        (when (and (eql (ltd:resize-event-width event) (ltd:display-width display))
                   (eql (ltd:resize-event-height event) (ltd:display-height display)))
          (session-resize session (ltd:display-rows display) (ltd:display-columns display)
                          :x11-configure-p t))))
    (sb-thread:with-mutex ((session-lock session))
      (setf (session-force-frame session) t)))
  (cond ((ltd:resize-event-interactive-p event)
         (ltd:cancel-size-request display)
         (session-event (host-session host) event))
        ;; While a transaction is active, dimensions do not prove configure
        ;; causality, so the engine and PTY keep the requested grid. Terminal
        ;; phases (:refused, :timed-out) are over: later configures report the
        ;; actual window and resize the grid, preserving content. The failed
        ;; request keeps its phase, so its late settlement is still ignored.
        ((ltd:geometry-active-p display) nil)
        (t (session-event (host-session host) event))))
