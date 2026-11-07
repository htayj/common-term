;;;; Public, bounded host lifecycle. No primary-process stdio changes here.
(in-package #:lispterm)

(defvar *hosts* (make-hash-table))
(defvar *hosts-lock* (sb-thread:make-mutex :name "lispterm hosts"))
(defvar *next-host-id* 0)

(defun stop-window-worker (host &optional startup-command)
  ;; Startup failure/cancellation exits the owner directly. Do not enqueue a
  ;; stop behind initialization, or close its PTY before that owner has joined.
  (when startup-command
    (sb-thread:with-mutex ((host-lock host))
      (unless (eq (host-command-state startup-command) :done)
        (setf (host-command-state startup-command) :cancelled))
      (setf (host-stop-p host) t)))
  (when (host-thread host)
    (when (and (not startup-command) (sb-thread:thread-alive-p (host-thread host)))
      (call-host host :stop))
    (unless (sb-thread:join-thread (host-thread host) :timeout 5 :default nil)
      (when (sb-thread:thread-alive-p (host-thread host))
        (error 'host-timeout :operation :join :detail "Window owner did not stop"))))
  (when (host-display host)
    (error 'host-error :operation :join :host host :detail "Native display cleanup is pending"))
  (sb-thread:with-mutex ((host-lock host)) (setf (host-thread host) nil)))

(defun open (&key argv pty (backend :auto) display-name (title "Lispterm") (rows 24) (columns 80))
  "Open a graphical PTY host. Supplying PTY adopts it without spawning a child."
  (unless (member backend '(:auto :x11 :wayland))
    (error 'host-error :operation :open :detail "Backend must be :auto, :x11, or :wayland"))
  (unless (and (typep rows '(integer 1 300)) (typep columns '(integer 1 500)))
    (error 'host-error :operation :open :detail "Grid exceeds 500 columns or 300 rows"))
  (when (and pty argv)
    (error 'host-error :operation :open :detail "Specify either PTY or ARGV"))
  (when pty
    (check-type pty lispterm.pty:pty)
    (unless (lispterm.pty:pty-open-p pty)
      (error 'host-error :operation :open :detail "Supplied PTY is closed")))
  (let* ((created-p (null pty))
         (pty (or pty (lispterm.pty:open-pty :rows rows :columns columns)))
         (host (%make-host :id (sb-thread:with-mutex (*hosts-lock*) (incf *next-host-id*))
                          :session (make-session pty rows columns)
                          :configuration (list :backend backend :display-name display-name :title title)))
         (startup (make-host-command
                   :operation :startup
                   :argument (when created-p
                               (or argv (list (or (uiop:getenv "SHELL") "/bin/sh")))))))
    (handler-case
        (progn
          (start-window-worker host startup)
          (wait-host-command host startup 5)
          (sb-thread:with-mutex (*hosts-lock*) (setf (gethash (host-id host) *hosts*) host))
          host)
      (error (condition)
        (handler-case (stop-window-worker host startup)
          (error (cleanup-error)
            ;; Retain a recovery handle if native cleanup is still pending.
            ;; Closing the PTY under a live owner would orphan the session.
            (host-record-error host :open-cleanup cleanup-error)
            (sb-thread:with-mutex (*hosts-lock*)
              (setf (gethash (host-id host) *hosts*) host))
            (error 'host-error :operation :open-cleanup :host host
                   :detail (format nil "Opening failed (~A); cleanup is pending (~A)"
                                   condition cleanup-error))))
        (when created-p (lispterm.pty:close-pty pty))
        (error condition)))))

(defun close (host)
  "Close and reap. Signal INPUT-CANCELLED if accepted input was undelivered."
  (let ((cancelled 0))
    (sb-thread:with-mutex ((host-control-lock host))
      (unless (eq (host-state-value host) :closed)
        (stop-window-worker host)
        (lispterm.pty:close-pty (session-pty (host-session host)))
        (let ((session (host-session host)))
          (sb-thread:with-mutex ((session-lock session))
            (sb-thread:with-mutex ((session-input-lock session))
              (setf cancelled (+ (session-input-pending session)
                                (loop for reply in (nerimux/terminal:screen-response-queue
                                                    (session-screen session))
                                      sum (length (utf8 reply))))
                    (session-input-head session) nil (session-input-tail session) nil
                    (session-input-offset session) 0 (session-input-pending session) 0
                    (nerimux/terminal:screen-response-queue (session-screen session)) nil))))
        (set-host-state host :closed)
        (sb-thread:with-mutex (*hosts-lock*) (remhash (host-id host) *hosts*))))
    (when (plusp cancelled)
      (let ((condition (make-condition 'input-cancelled :operation :close :host host
                                      :bytes cancelled
                                      :detail (format nil "Closed with ~D undelivered input bytes" cancelled))))
        (host-record-error host :input-cancelled condition)
        (error condition))))
  host)

(defun status (&optional host)
  "Return a fresh plist for HOST, or snapshots of all registered hosts."
  (if (null host)
      (mapcar #'status (sb-thread:with-mutex (*hosts-lock*)
                        (loop for host being the hash-values of *hosts* collect host)))
      (let ((session (host-session host)))
        (append
         (sb-thread:with-mutex ((host-lock host))
           (list :id (host-id host) :state (host-state host)
                 :backend (host-effective-backend host)
                 :worker-alive-p (and (host-thread host) (sb-thread:thread-alive-p (host-thread host)))
                 :error-count (length (host-error-ring host))))
         (sb-thread:with-mutex ((session-lock session))
           (list :rows (lispterm.engine:rows (session-screen session))
                 :columns (lispterm.engine:columns (session-screen session))
                 :engine-geometry-generation
                 (nerimux/terminal/types:screen-geometry-generation (session-screen session))
                 :pty-rows (session-pty-rows session) :pty-columns (session-pty-columns session)
                 :geometry (copy-tree (session-geometry-status session))
                 :frames (session-frames session)
                :blink-phase (session-blink-phase session)
                :blink-visible (session-blink-visible session) :bytes-received (session-bytes-received session)
                 :transport-state (session-transport-state session)
                 :process-state (session-process-state session)
                 :child-pid (let ((process (lispterm.pty:pty-process (session-pty session))))
                               (and process (lispterm.pty:process-pid process)))))
           (sb-thread:with-mutex ((session-input-lock session))
             (list :input-pending (session-input-pending session)
                   :input-limit *input-queue-limit*))))))

(defun reopen (host)
  "Queue display replacement on its owner, retaining PTY and engine."
  (sb-thread:with-mutex ((host-control-lock host)) (call-host host :reopen))
  host)

(defun invalidate (host)
  "Request one fresh frame, including after live renderer redefinition."
  (sb-thread:with-mutex ((host-control-lock host)) (call-host host :invalidate))
  host)

(defun send-input (host input)
  "Accept text or octets into the bounded owner FIFO, or signal backpressure."
  (when (member (host-state-value host) '(:quiesced :closed :saved))
    (error 'host-error :operation :input :detail "Host has no active input owner"))
  (let ((bytes (if (stringp input) (utf8 input) (copy-seq input))))
    (unless (<= (length bytes) 65536)
      (error 'host-error :operation :input :detail "Input exceeds 65536 bytes"))
    (sb-thread:with-mutex ((host-control-lock host)) (call-host host :input bytes)))
  host)

(defun quiesce (host)
  "Stop and join the owner, preserving PTY and engine for rollback."
  (sb-thread:with-mutex ((host-control-lock host))
    (when (member (host-state-value host) '(:closed :saved))
      (error 'host-error :operation :quiesce :detail "Host is closed"))
    (let ((token (make-lifecycle-token :host host :generation (1+ (host-generation host))
                                     :attached-p (eq (host-state-value host) :attached))))
      ;; Stop acknowledgement follows native close. If it fails, the original
      ;; owner keeps running. Thus partial close cannot strand a stopped worker.
      (stop-window-worker host)
      (incf (host-generation host))
      (set-host-state host :quiesced)
      token)))

(defun resume (host token)
  "Resume a quiesced session using its exact rollback token."
  (sb-thread:with-mutex ((host-control-lock host))
    (unless (and (lifecycle-token-p token) (eq host (lifecycle-token-host token))
                 (= (host-generation host) (lifecycle-token-generation token)))
      (error 'host-error :operation :resume :detail "Stale or foreign lifecycle token"))
    (unless (lifecycle-token-used-p token)
      (unless (eq (host-state-value host) :quiesced)
        (error 'host-error :operation :resume :detail "Host is not quiesced"))
      (start-window-worker host)
      (set-host-state host :detached)
      (setf (lifecycle-token-used-p token) t)
      (when (lifecycle-token-attached-p token) (call-host host :reopen))))
  host)

(defun detach-for-saver (host)
  "In a post-fork saver child, release inherited handles only. Never join/reap."
  ;; No inherited mutex may be acquired: another parent-side observer could
  ;; have held it at fork. The saver is exclusively responsible for this copy.
  (unless (and (eq (host-state host) :quiesced) (null (host-display host))
               (null (host-thread host)))
    (error 'host-error :operation :detach-for-saver :detail "Host must be quiesced before fork"))
  (lispterm.pty:detach-pty (session-pty (host-session host)))
  (setf (host-lock host) (sb-thread:make-mutex :name "saved lispterm host")
        (host-control-lock host) (sb-thread:make-mutex :name "saved lispterm lifecycle")
        (session-lock (host-session host)) (sb-thread:make-mutex :name "saved lispterm screen")
        (session-input-lock (host-session host)) (sb-thread:make-mutex :name "saved lispterm input")
        (host-state host) :saved)
  (setf *hosts-lock* (sb-thread:make-mutex :name "saved lispterm registry"))
  (remhash (host-id host) *hosts*)
  host)

(defun retry-geometry (host)
  "Retry the current engine grid intent on the owner without replacing the PTY."
  (call-host host :geometry-retry) host)
(defun cancel-geometry (host)
  "Release a geometry request and adopt the actual display grid, preserving content."
  (call-host host :geometry-cancel) host)
(export '(retry-geometry cancel-geometry))
