;;;; Native Lisp X11 transport. Pixel storage and glyphs belong to the shared core.

(in-package #:lispterm.display.x11)

(defconstant +primary-paste-limit+ (* 1024 1024))

(defstruct primary-paste
  time target deadline reading-p)

(defstruct x-state
  client
  window
  gc
  depth
  bpp
  pad
  byte-order
  red-mask
  green-mask
  blue-mask
  events
  refreshing
  pending-keys geometry-next geometry-query-p geometry-sent-size configure-sequence
  paste-property utf8-string primary-paste)

(defun queue-event (d event) (push event (x-state-events (display-backend-state d))))

(defun atom-id (c name)
  (let* ((bytes (babel:string-to-octets name :encoding :utf-8))
         (reply (xclhb:intern-atom-sync c 0 (length bytes) bytes)))
    (xclhb:intern-atom-reply-atom reply)))

(defun translate-keysyms (symbols modifiers)
  "Translate a core mapping while retaining keypad identity in either level."
  (let* ((base (first symbols))
         (shift (logbitp 0 modifiers)))
    (when base
      (when (and (logbitp 1 modifiers) (< base 256) (alpha-char-p (code-char base)))
        (setf shift (not shift)))
      (let ((keysym
             (or (and shift (second symbols) (not (zerop (second symbols))) (second symbols))
                 base)))
        (or (lispterm.display::keysym-keypad-key keysym)
            (cond ((< keysym 256) (code-char keysym))
                  ((<= 16777472 keysym 17891327) (code-char (- keysym 16777216)))
                  (t (cdr (assoc keysym xclhb::+keycode-keysym-table-non-character+)))))))))

(defun translate-key (client keycode modifiers)
  (translate-keysyms (cdr (assoc keycode (xclhb:client-keycode-keysym-table client)))
                    modifiers))

(defun finish-primary-paste (state)
  (setf (x-state-primary-paste state) nil)
  (let ((client (x-state-client state)))
    (xclhb:delete-property client (x-state-window state) (x-state-paste-property state))
    (xclhb:flush client)))

(defun convert-primary (state paste target)
  (setf (primary-paste-target paste) target)
  (let ((client (x-state-client state)))
    (xclhb:convert-selection client (x-state-window state) xclhb:+atom--primary+
                             target (x-state-paste-property state) (primary-paste-time paste))
    (xclhb:flush client)))

(defun request-primary-paste (state time)
  ;; Coalesce repeated Shift+Insert while one conversion is outstanding.
  (unless (x-state-primary-paste state)
    (let ((paste (make-primary-paste :time time
                                     :deadline (+ (get-internal-real-time)
                                                  (* 5 internal-time-units-per-second)))))
      (xclhb:delete-property (x-state-client state) (x-state-window state)
                             (x-state-paste-property state))
      (setf (x-state-primary-paste state) paste)
      (convert-primary state paste (x-state-utf8-string state)))))

(defun primary-paste-text (reply target)
  ;; A different type (including INCR), a partial read, or invalid text
  ;; is not a paste. X11 STRING is ISO-8859-1, not UTF-8.
  (when (and (= target (xclhb:get-property-reply-type reply))
             (= 8 (xclhb:get-property-reply-format reply))
             (zerop (xclhb:get-property-reply-bytes-after reply)))
    (let ((bytes (xclhb:get-property-reply-value reply)))
      (when (<= 1 (length bytes) +primary-paste-limit+)
        (handler-case
            (babel:octets-to-string bytes :encoding
                                   (if (= target xclhb:+atom--string+) :latin-1 :utf-8))
          (babel:character-decoding-error () nil))))))

(defun read-primary-paste (display state paste)
  (let ((client (x-state-client state))
        (target (primary-paste-target paste)))
    (setf (primary-paste-reading-p paste) t)
    (xclhb:get-property client
      (lambda (reply)
        (let ((text (primary-paste-text reply target)))
          (finish-primary-paste state)
          (when text (queue-event display (make-paste-event :text text)))))
      0 (x-state-window state) (x-state-paste-property state) target
      0 (/ +primary-paste-limit+ 4))
    (xclhb:flush client)))

(defun selection-notify (display event)
  (let* ((state (display-backend-state display))
         (paste (x-state-primary-paste state))
         (property (xclhb:selection-notify-event-property event)))
    (when (and paste (not (primary-paste-reading-p paste))
               (= (x-state-window state) (xclhb:selection-notify-event-requestor event))
               (= xclhb:+atom--primary+ (xclhb:selection-notify-event-selection event))
               (= (primary-paste-time paste) (xclhb:selection-notify-event-time event))
               (= (primary-paste-target paste) (xclhb:selection-notify-event-target event)))
      (cond ((= property (x-state-paste-property state)) (read-primary-paste display state paste))
            ((zerop property)
             (if (= (primary-paste-target paste) (x-state-utf8-string state))
                 (convert-primary state paste xclhb:+atom--string+)
                 (finish-primary-paste state)))))))

(defun dispatch-key (display event)
  (let* ((state (display-backend-state display)) (client (x-state-client state)))
    (if (x-state-refreshing state)
        (push event (x-state-pending-keys state))
        (let* ((modifiers (xclhb:key-press-event-state event))
               (key (translate-key client (xclhb:key-press-event-detail event) modifiers)))
          (when key
            (let ((semantic-modifiers
                    (loop for (bit name) in '((0 :shift) (2 :control) (3 :alt) (6 :super))
                          when (logbitp bit modifiers)
                          collect name)))
              (if (and (eq key :insert) (equal semantic-modifiers '(:shift)))
                  (request-primary-paste state (xclhb:key-press-event-time event))
                  (queue-event display
                   (make-key-event :key key
                    :text (or (lispterm.display::keypad-text key) (when (characterp key) (string key)))
                    :modifiers semantic-modifiers)))))))))

(defun refresh-keymap (display)
  (let ((state (display-backend-state display)))
    (unless (x-state-refreshing state)
      (setf (x-state-refreshing state) t)
      (unwind-protect (xclhb:set-keycode-keysym-table (x-state-client state))
        (setf (x-state-refreshing state) nil))
      (let ((pending (nreverse (x-state-pending-keys state))))
        (setf (x-state-pending-keys state) nil)
        (dolist (event pending) (dispatch-key display event))))))

(defun set-window-identity (display client window)
  (let* ((name (display-window-class display))
         (class (babel:string-to-octets name :encoding :latin-1))
         (width (1+ (length class)))
         (value (make-array (* 2 width) :element-type '(unsigned-byte 8) :initial-element 0))
         (hostname (babel:string-to-octets (machine-instance) :encoding :latin-1)))
    (when (find 0 class)
      (error 'display-error :detail "Window class must not contain NUL"))
    (replace value class)
    (replace value class :start1 width)
    (xclhb:change-property client xclhb:+prop-mode--replace+ window xclhb:+atom--wm-class+
                           xclhb:+atom--string+ 8 (length value) value)
    (xclhb:change-property client xclhb:+prop-mode--replace+ window (atom-id client "_NET_WM_PID")
                           xclhb:+atom--cardinal+ 32 1
                           (xclhb:card32->card8-vector (sb-unix:unix-getpid)))
    (xclhb:change-property client xclhb:+prop-mode--replace+ window xclhb:+atom--wm-client-machine+
                           xclhb:+atom--string+ 8 (length hostname) hostname)))
(defmethod backend-open ((backend (eql :x11)) d)
  (let* ((c (or (xclhb:x-connect nil (display-name d)) (error "X connection rejected")))
         (state (setf (display-backend-state d) (make-x-state :client c)))
         (info (xclhb:client-server-information c))
         (screen (elt (xclhb:setup-roots info) 0))
         (depth (xclhb:screen-root-depth screen))
         (fmt (find depth (xclhb:setup-pixmap-formats info) :key #'xclhb:format-depth))
         (visual
          (loop for dep across (xclhb:screen-allowed-depths screen)
                thereis (find (xclhb:screen-root-visual screen) (xclhb:depth-visuals dep) :key
                              #'xclhb:visualtype-visual-id)))
         (win (xclhb:allocate-resource-id c))
         (gc (xclhb:allocate-resource-id c)))
    (setf (display-backend-state d) state
          (x-state-window state) win
          (x-state-gc state) gc
          (x-state-depth state) depth
          (x-state-bpp state) (xclhb:format-bits-per-pixel fmt)
          (x-state-pad state) (xclhb:format-scanline-pad fmt)
          (x-state-byte-order state) (xclhb:setup-image-byte-order info)
          (x-state-red-mask state) (xclhb:visualtype-red-mask visual)
          (x-state-green-mask state) (xclhb:visualtype-green-mask visual)
          (x-state-blue-mask state) (xclhb:visualtype-blue-mask visual))
    (unless
        (and (member (x-state-bpp state) '(16 24 32)) (= 4 (xclhb:visualtype-class visual)))
      (error "X11 requires a TrueColor 16/24/32-bit visual"))
    (xclhb:set-default-error-handler c (lambda (e) (error 'display-error :detail e)))
    (apply #'xclhb:create-window c depth win (xclhb:screen-root screen) 0 0 (display-width d)
           (display-height d) 0 1 (xclhb:screen-root-visual screen) (ash 1 11)
           (append (make-list 11 :initial-element 0)
                   (list (logior 1 (ash 1 15) (ash 1 17) (ash 1 21)))
                   (make-list 3 :initial-element 0)))
    (apply #'xclhb:create-gc c gc win 0 (make-list 23 :initial-element 0))
    (xclhb:set-keycode-keysym-table c)
    (setf (x-state-paste-property state) (atom-id c "_COMMON_TERM_PRIMARY")
          (x-state-utf8-string state) (atom-id c "UTF8_STRING"))
    (xclhb:set-event-handler c 31 (lambda (e) (selection-notify d e)))
    (xclhb:set-event-handler c 2 (lambda (e) (dispatch-key d e)))
    (xclhb:set-event-handler c 12
                             (lambda (e)
                               (declare (ignore e))
                               (setf (display-damage d)
                                       (list
                                        (list 0 0 (display-width d) (display-height d))))))
    (xclhb:set-event-handler c 22
      (lambda (e)
        ;; The generated event drops the SendEvent bit and the sequence. The
        ;; input buffer still contains the current event; sequence is diagnostic.
        (let ((bytes (xclhb:client-input-buffer c)))
          (x11-handle-configure d (xclhb:configure-notify-event-window e)
            (xclhb:configure-notify-event-width e) (xclhb:configure-notify-event-height e)
            (xclhb::read-card16 bytes 2) (logbitp 7 (aref bytes 0))))))
    (xclhb:set-event-handler c 9
                             (lambda (e)
                               (declare (ignore e))
                               (queue-event d (make-focus-event :focused-p t))))
    (xclhb:set-event-handler c 10
                             (lambda (e)
                               (declare (ignore e))
                               (queue-event d (make-focus-event :focused-p nil))))
    (xclhb:set-event-handler c 17
                             (lambda (e)
                               (declare (ignore e))
                               (queue-event d (make-close-event))))
    (xclhb:set-event-handler c 34 (lambda (e) (declare (ignore e)) (refresh-keymap d)))
    (let ((delete (atom-id c "WM_DELETE_WINDOW")))
      (xclhb:change-property c 0 win (atom-id c "WM_PROTOCOLS") 4 32 1
                             (xclhb:card32->card8-vector delete))
      (xclhb:set-event-handler c 33
                               (lambda (e)
                                 (when (= delete (aref (xclhb:client-message-event-data e) 0))
                                   (queue-event d (make-close-event))))))
    (backend-set-title backend d (display-title d))
    (set-window-identity d c win)
    (xclhb:map-window c win)
    (xclhb:flush c)))

(defmethod backend-close ((backend (eql :x11)) d)
  (let ((s (display-backend-state d)))
    (when s
      ;; The auto-close stream owns the socket; disconnect also frees its X
      ;; resources. Retain the client and resource IDs on any nonlocal exit so
      ;; CLOSE-DISPLAY can retry cleanup rather than declaring quiescence.
      (close (xclhb:client-stream (x-state-client s)) :abort t)
      (setf (display-backend-state d) nil))))

(defun process-packet (c)
  (let ((buffer (xclhb:client-input-buffer c)))
    (unless (= 32 (read-sequence buffer (xclhb:client-stream c) :end 32))
      (error 'display-connection-lost :detail "X11 peer closed"))
    (let ((type (aref buffer 0)))
      (case type
        (0 (xclhb::process-error c buffer))
        (1 (xclhb::process-reply c buffer))
        (otherwise (xclhb::process-event c type buffer))))))

(defmethod backend-poll ((backend (eql :x11)) d timeout)
  (handler-case
   (let* ((s (display-backend-state d))
          (c (x-state-client s))
          (stream (xclhb:client-stream c))
          (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
       ;; An unresponsive owner must not permanently disable PRIMARY paste.
       (let ((paste (x-state-primary-paste s)))
         (when (and paste (not (primary-paste-reading-p paste))
                    (>= (get-internal-real-time) (primary-paste-deadline paste)))
           (finish-primary-paste s)))
     (loop do (loop while (or (listen stream)
                              (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream) :input
                                                           0))
                    do (process-packet c))
           when (or (x-state-events s) (>= (get-internal-real-time) deadline)) return (prog1
                                                                                          (nreverse
                                                                                           (x-state-events
                                                                                            s))
                                                                                        (setf (x-state-events
                                                                                               s)
                                                                                                nil))
           do (sleep (min 0.005 timeout))))
   (stream-error (e) (error 'display-connection-lost :detail e))))

(defun channel-pixel (value mask)
  (let ((shift (1- (integer-length (logand mask (- mask))))))
    (ash (round (* value (ash mask (- shift))) 255) shift)))

(defmethod backend-blit ((backend (eql :x11)) d framebuffer rectangles)
  (declare (ignore rectangles))
  (handler-case
   (let* ((s (display-backend-state d))
          (c (x-state-client s))
          (w (display-width d))
          (h (display-height d))
          (bpp (x-state-bpp s))
          (stride (* (/ (x-state-pad s) 8) (ceiling (* w bpp) (x-state-pad s))))
          (limit
           (min 240000
                (-
                 (* 4 (xclhb:setup-maximum-request-length (xclhb:client-server-information c)))
                 32)))
          (lines (max 1 (floor limit stride))))
     (when (> stride limit)
       (error 'display-error :detail "X11 row exceeds negotiated request size"))
     (loop for y from 0 below h by lines
           for
           count = (min lines (- h y))
           do (let ((bytes
                     (make-array (* stride count) :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
                (dotimes (dy count)
                  (dotimes (x w)
                    (let* ((rgb (aref framebuffer (+ x (* (+ y dy) w))))
                           (pixel
                            (logior (channel-pixel (ldb (byte 8 16) rgb) (x-state-red-mask s))
                                    (channel-pixel (ldb (byte 8 8) rgb) (x-state-green-mask s))
                                    (channel-pixel (ldb (byte 8 0) rgb)
                                     (x-state-blue-mask s))))
                           (p (+ (* dy stride) (* x (/ bpp 8)))))
                      (dotimes (i (/ bpp 8))
                        (setf (aref bytes (+ p i))
                                (ldb
                                 (byte 8
                                       (* 8
                                          (if (zerop (x-state-byte-order s))
                                              i
                                              (- (/ bpp 8) i 1))))
                                 pixel))))))
                (xclhb:put-image c 2 (x-state-window s) (x-state-gc s) w count 0 y 0
                                 (x-state-depth s) bytes)))
     (x11-geometry-presented d)
     (xclhb:flush c))
   (stream-error (e) (error 'display-connection-lost :detail e))))

(defmethod backend-set-title ((backend (eql :x11)) d title)
  (let* ((s (display-backend-state d))
         (c (x-state-client s))
         (bytes (babel:string-to-octets title :encoding :utf-8)))
    (xclhb:change-property c 0 (x-state-window s) 39 31 8 (length bytes) bytes)
    (xclhb:change-property c 0 (x-state-window s) (atom-id c "_NET_WM_NAME")
                           (atom-id c "UTF8_STRING") 8 (length bytes) bytes)
    (xclhb:flush c)))

(defmethod backend-metrics ((backend (eql :x11)) d)
  (let ((s (display-backend-state d)))
    (when s
      (list :configure-sequence (x-state-configure-sequence s)
            :window-id (x-state-window s) :depth (x-state-depth s) :bits-per-pixel
            (x-state-bpp s)))))

(defun x11-target-matches-p (request width height)
  (and request (= width (lispterm.display:geometry-request-width request))
       (= height (lispterm.display:geometry-request-height request))))
(defun x11-geometry-configured (display width height)
  (let ((request (ltd:display-geometry-request display)))
    (when (and request (eq (ltd:geometry-request-phase request) :pending)
               (x11-target-matches-p request width height))
      (ltd:geometry-result display request :surface-ready)
      (setf (display-damage display) (list (list 0 0 width height))))))

(defun x11-handle-configure (display window width height sequence synthetic-p)
  (let ((s (display-backend-state display)))
    (when (and (not synthetic-p) (eql window (x-state-window s))
               (not (and (= width (display-width display)) (= height (display-height display)))))
      (resize-display display width height)
      (let ((request (ltd:display-geometry-request display)))
        (setf (x-state-configure-sequence s) sequence)
        (when (equal (x-state-geometry-sent-size s) (cons width height))
          (setf (x-state-geometry-sent-size s) nil))
        (queue-event display (make-resize-event :width width :height height
                               :rows (display-rows display) :columns (display-columns display)
                               :serial sequence))
        ;; A size change is actual even if it does not match the current
        ;; request. X event order does not tell us whether a WM is done.
        (if (x11-target-matches-p request width height)
            (x11-geometry-configured display width height)
            (when (and request (member (ltd:geometry-request-phase request) '(:surface-ready :closing)))
              (ltd:geometry-result display request :pending)))))))

(defun x11-start-size-request (display)
  (let* ((s (display-backend-state display)) (request (x-state-geometry-next s)))
    (when request
      (let* ((c (x-state-client s))
             (width (ltd:geometry-request-width request))
             (height (ltd:geometry-request-height request))
             (sent (x-state-geometry-sent-size s)))
        (cond ((or (not (x11-target-matches-p request (display-width display) (display-height display)))
                   (and sent (not (equal sent (cons width height)))))
               ;; A still-outstanding different target can make the cached
               ;; local size stale. Send a reverse even if the cache matches it.
               (setf (x-state-geometry-next s) nil (x-state-geometry-sent-size s) (cons width height))
               (xclhb:configure-window c (x-state-window s)
                                      (logior (ash 1 xclhb:+config-window--width+)
                                              (ash 1 xclhb:+config-window--height+))
                                      0 0 width height 0 0 0)
               (xclhb:flush c))
              ((not (x-state-geometry-query-p s))
               (setf (x-state-geometry-next s) nil (x-state-geometry-query-p s) t)
               ;; Query only a possible no-op. Never use a reply to publish a
               ;; changed size: a genuine size ConfigureNotify must do that.
               (xclhb:get-geometry c
                 (lambda (reply)
                   (setf (x-state-geometry-query-p s) nil)
                   (when (and (ltd:geometry-current-p display request)
                              (x11-target-matches-p request (display-width display) (display-height display))
                              (x11-target-matches-p request (xclhb:get-geometry-reply-width reply)
                                                              (xclhb:get-geometry-reply-height reply)))
                     (x11-geometry-configured display width height))
                   (x11-start-size-request display))
                 (x-state-window s))
               (xclhb:flush c)))))))
(defmethod lispterm.display:backend-request-size ((backend (eql :x11)) display request)
  (declare (ignore backend))
  (setf (x-state-geometry-next (display-backend-state display)) request)
  (x11-start-size-request display))
(defmethod lispterm.display:backend-cancel-size ((backend (eql :x11)) display request)
  (declare (ignore backend request))
  (let ((s (display-backend-state display)))
    (when s (setf (x-state-geometry-next s) nil))))
(defun x11-geometry-presented (display)
  (let ((r (lispterm.display:display-geometry-request display))
        (s (display-backend-state display)))
    (when (and (not (x-state-geometry-query-p s))
               (eq :surface-ready (getf (lispterm.display:geometry-status display) :phase))
               (x11-target-matches-p r (display-width display) (display-height display)))
      (setf (x-state-geometry-query-p s) t)
      (lispterm.display:geometry-result display r :closing)
      ;; Ordered after PutImage: server-compatible, not externally visible.
      (xclhb:get-geometry (x-state-client s)
        (lambda (reply)
          (setf (x-state-geometry-query-p s) nil)
          (when (and (ltd:geometry-current-p display r)
                     (eq (ltd:geometry-request-phase (ltd:display-geometry-request display)) :closing)
                     (x11-target-matches-p r (xclhb:get-geometry-reply-width reply)
                                           (xclhb:get-geometry-reply-height reply))
                     (x11-target-matches-p r (display-width display) (display-height display)))
            (incf (lispterm.display:display-presentation-generation display))
            (lispterm.display:geometry-result display r :settled :server-compatible))
          (x11-start-size-request display))
        (x-state-window s)))))
