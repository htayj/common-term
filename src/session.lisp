;;;; Session state survives replacement of the graphical window.
(in-package #:lispterm)

(define-condition host-error (lispterm-error)
  ((host :initarg :host :initform nil :reader host-error-host)))
(define-condition host-timeout (host-error) ())

(define-condition input-backpressure (host-error)
  ((pending :initarg :pending :reader input-backpressure-pending)
   (requested :initarg :requested :reader input-backpressure-requested)))
(define-condition input-cancelled (host-error)
  ((bytes :initarg :bytes :reader input-cancelled-bytes)))

(defparameter *input-queue-limit* 262144)

(defstruct (session (:constructor %make-session))
  pty screen
  (geometry-generation 0) geometry-intent geometry-status pty-rows pty-columns
  ;; (rows . columns) from actual window geometry, or NIL outside X11.
  x11-grid
  (lock (sb-thread:make-mutex :name "lispterm screen"))
  (transport-state :open) process-state
  (bytes-received 0) (frames 0) (force-frame t) last-cursor last-title
  (blink-phase t) (blink-visible nil) next-blink
  ;; Blink receipts, published with their completed frame. Times are exact
  ;; rational seconds of the monotonic internal real time at toggle decisions.
  ;; BLINK-EVENT-AT is the latest reset or toggle of the current epoch.
  (blink-epoch 0) blink-epoch-at (blink-toggles 0) blink-toggled-at blink-previous-at
  blink-toggle-frame blink-event-at
  ;; The window owner alone enqueues/drains. Observers use INPUT-LOCK.
  (input-lock (sb-thread:make-mutex :name "lispterm input"))
  input-head input-tail (input-offset 0) (input-pending 0))

(defun make-session (pty rows columns)
  (multiple-value-bind (pty-rows pty-columns)
      (when pty (lispterm.pty:pty-window-size pty))
    (%make-session :pty pty :screen (lispterm.engine:make-screen :rows rows :columns columns)
                   :pty-rows pty-rows :pty-columns pty-columns)))

(defun session-blink-receipt (session)
  "Blink state of the latest completed frame; call with SESSION-LOCK held."
  (list :blink-phase (session-blink-phase session)
        :blink-visible (session-blink-visible session)
        :blink-epoch (session-blink-epoch session)
        :blink-epoch-at (session-blink-epoch-at session)
        :blink-toggles (session-blink-toggles session)
        :blink-toggled-at (session-blink-toggled-at session)
        :blink-previous-at (session-blink-previous-at session)
        :blink-toggle-frame (session-blink-toggle-frame session)))

(defun utf8 (text)
  (babel:string-to-octets text :encoding :utf-8))

(defvar *enqueueing-response* nil)
(defun session-write (session bytes)
  "Accept a private copy into the bounded FIFO, not a delivery receipt."
  (check-type bytes (vector (unsigned-byte 8)))
  (sb-thread:with-mutex ((session-input-lock session))
    (let ((count (length bytes)))
      (when (or (> (+ (session-input-pending session) count) *input-queue-limit*)
                (and (not *enqueueing-response*)
                     (nerimux/terminal:screen-response-queue (session-screen session))))
        (error 'input-backpressure :operation :input
               :pending (session-input-pending session) :requested count
               :detail "Pending PTY input queue is full; retry after draining"))
      (when (plusp count)
        (let ((entry (list (copy-seq bytes))))
          (if (session-input-tail session)
              (setf (cdr (session-input-tail session)) entry)
              (setf (session-input-head session) entry))
          (setf (session-input-tail session) entry)
          (incf (session-input-pending session) count)
          (lispterm.trace:trace-event :input-enqueue :bytes count
            :response-p *enqueueing-response*)))
      count)))

(defun session-flush-input (session &key (max-writes 1) (max-bytes 4096))
  "Drain bounded nonblocking prefixes. Return bytes accepted and drain status.
A zero write ends this attempt immediately; a positive partial write may retry."
  (sb-thread:with-mutex ((session-input-lock session))
    (let ((total 0))
      (dotimes (attempt max-writes)
        (declare (ignorable attempt))
        (unless (session-input-head session)
          (return-from session-flush-input (values total :empty)))
        (when (>= total max-bytes)
          (return-from session-flush-input (values total :budget)))
        (let* ((bytes (car (session-input-head session)))
               (offset (session-input-offset session))
               (chunk (subseq bytes offset
                              (min (length bytes) (+ offset 4096)
                                   (+ offset (- max-bytes total)))))
               (count (lispterm.pty:pty-write (session-pty session) chunk :timeout 0)))
          (unless (<= 0 count (length chunk))
            (error 'host-error :operation :input :detail "Invalid PTY accepted prefix"))
          (incf (session-input-offset session) count)
          (decf (session-input-pending session) count)
          (incf total count)
          (lispterm.trace:trace-event :pty-write :accepted count :offered (length chunk))
          (when (= (session-input-offset session) (length bytes))
            (setf (session-input-head session) (cdr (session-input-head session))
                  (session-input-offset session) 0)
            (unless (session-input-head session) (setf (session-input-tail session) nil)))
          (when (zerop count)
            (return-from session-flush-input (values total :blocked)))))
      (values total (if (session-input-head session) :budget :empty)))))

(defun session-queue-responses (session)
  ;; Keep engine replies until the complete batch fits. Stop reading more output
  ;; while backpressured, bounding the unqueued batch to one PTY read's replies.
  (sb-thread:with-mutex ((session-lock session))
    (let* ((screen (session-screen session))
           (replies (reverse (nerimux/terminal:screen-response-queue screen))))
      (when replies
        (let ((count (loop for reply in replies sum (length (utf8 reply)))))
          (sb-thread:with-mutex ((session-input-lock session))
            (when (> (+ count (session-input-pending session)) *input-queue-limit*)
              (return-from session-queue-responses nil))))
        (let ((*enqueueing-response* t))
          (dolist (reply replies) (session-write session (utf8 reply))))
        (setf (nerimux/terminal:screen-response-queue screen) nil)))
    t))

(defun session-reported-grid (session)
  "Return actual rows and columns. Call with SESSION-LOCK held."
  (let ((grid (session-x11-grid session)) (screen (session-screen session)))
    (if grid (values (car grid) (cdr grid))
        (values (lispterm.engine:rows screen) (lispterm.engine:columns screen)))))

(defun session-screen-size-p (session rows columns)
  (let ((screen (session-screen session)))
    (and (= rows (lispterm.engine:rows screen)) (= columns (lispterm.engine:columns screen)))))

(defun session-restore-x11-grid (session)
  "Discard a speculative size, retaining content where it fits. Call under SESSION-LOCK."
  (let ((grid (session-x11-grid session)))
    (when (and grid (not (session-screen-size-p session (car grid) (cdr grid))))
      (lispterm.engine:resize (session-screen session) (car grid) (cdr grid))
      (setf (session-force-frame session) t))))
(defun session-observe-geometry (session)
  ;; Called with SESSION-LOCK after a complete engine feed. No second resize:
  ;; characters following DECCOLM have already used the new stride.
  (let* ((screen (session-screen session))
         (generation (nerimux/terminal/types:screen-geometry-generation screen)))
    (unless (= generation (session-geometry-generation session))
      (let ((rows (lispterm.engine:rows screen))
            (columns (lispterm.engine:columns screen)))
        (unless (session-x11-grid session)
          (lispterm.pty:pty-resize (session-pty session) rows columns)
          (setf (session-pty-rows session) rows (session-pty-columns session) columns))
        (setf (session-geometry-generation session) generation
              (session-geometry-intent session) (list generation rows columns)
              (session-force-frame session) t)))))

(defun session-read-output (session)
  "Drain one input chunk and read one output chunk per scheduler iteration."
  (session-flush-input session)
  (unless (session-queue-responses session) (return-from session-read-output nil))
  (let* ((pty (session-pty session))
         (bytes (let ((bytes (lispterm.pty:pty-read pty :timeout 0)))
                  (when (vectorp bytes)
                    (lispterm.trace:trace-event :pty-read :bytes (length bytes)))
                  bytes))
         (process (lispterm.pty:pty-process pty))
         (process-state (and process (lispterm.pty:process-poll process))))
    (sb-thread:with-mutex ((session-lock session))
      (setf (session-process-state session) process-state)
      (cond ((eq bytes :eof)
             (setf (session-transport-state session)
                   (if process :hangup :waiting-for-slave)))
            (bytes
             (setf (session-transport-state session) :open)
             (incf (session-bytes-received session) (length bytes))
               (lispterm.engine:feed (session-screen session) bytes)
             (lispterm.trace:trace-event :feed-done :bytes (length bytes)
               :screen (session-screen session))
             (session-observe-geometry session)
             (setf (nerimux/terminal:screen-passthrough-queue (session-screen session)) nil
                   (nerimux/terminal:screen-clipboard-queue (session-screen session)) nil
                   (nerimux/terminal:screen-notification-queue (session-screen session)) nil))))
    (session-queue-responses session)
    bytes))

(defun session-resize (session rows columns &key x11-configure-p)
  ;; Bound allocations from compositor or client supplied geometry.
  (unless (and (typep rows '(integer 1 300)) (typep columns '(integer 1 500)))
    (error 'host-error :operation :resize :detail "Grid exceeds 500 columns or 300 rows"))
  (sb-thread:with-mutex ((session-lock session))
    (cond (x11-configure-p
           (unless (and (eql rows (session-pty-rows session))
                        (eql columns (session-pty-columns session)))
             (when (session-pty session)
               (lispterm.pty:pty-resize (session-pty session) rows columns))
             (setf (session-pty-rows session) rows (session-pty-columns session) columns))
           (setf (session-x11-grid session) (cons rows columns))
           (session-restore-x11-grid session))
          ((not (session-x11-grid session))
           (lispterm.pty:pty-resize (session-pty session) rows columns)
           (lispterm.engine:resize (session-screen session) rows columns)
           (setf (session-pty-rows session) rows (session-pty-columns session) columns)))
    (setf (session-force-frame session) t)))

(defun session-event (session event)
  (typecase event
    (ltd:resize-event
     (session-resize session (ltd:resize-event-rows event) (ltd:resize-event-columns event)))
    (ltd:key-event
     (sb-thread:with-mutex ((session-lock session))
       (let ((screen (session-screen session)))
         (session-write session
           (ltd:key-event-bytes event
             :application-cursor (nerimux/terminal:screen-app-cursor-keys screen)
             :application-keypad (nerimux/terminal:screen-app-keypad screen)
             :vt52 (nerimux/terminal:screen-vt52-mode screen))))))
    (ltd:text-event (session-write session (utf8 (ltd:text-event-text event))))
    (ltd:paste-event
     (let ((text (ltd:paste-event-text event)))
       (sb-thread:with-mutex ((session-lock session))
         (session-write session
           (utf8 (if (nerimux/terminal:screen-bracketed-paste (session-screen session))
                     (format nil "~c[200~~~a~c[201~~" #\Esc text #\Esc) text))))))
    (ltd:focus-event
     (sb-thread:with-mutex ((session-lock session))
       (when (nerimux/terminal/types:screen-focus-events (session-screen session))
         (session-write session (utf8 (format nil "~c[~a" #\Esc
                           (if (ltd:focus-event-focused-p event) "I" "O")))))))))
