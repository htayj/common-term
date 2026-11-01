;;;; Native Lisp X11 transport. Pixel storage and glyphs belong to the shared core.

(in-package #:lispterm.display.x11)

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
  pending-keys geometry-next geometry-query-p configure-sequence)

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

(defun dispatch-key (display event)
  (let* ((state (display-backend-state display)) (client (x-state-client state)))
    (if (x-state-refreshing state)
        (push event (x-state-pending-keys state))
        (let* ((modifiers (xclhb:key-press-event-state event))
               (key (translate-key client (xclhb:key-press-event-detail event) modifiers)))
          (when key
            (queue-event display
             (make-key-event :key key
              :text (or (lispterm.display::keypad-text key) (when (characterp key) (string key)))
              :modifiers
              (loop for (bit name) in '((0 :shift) (2 :control) (3 :alt) (6 :super))
                    when (logbitp bit modifiers)
                    collect name))))))))

(defun refresh-keymap (display)
  (let ((state (display-backend-state display)))
    (unless (x-state-refreshing state)
      (setf (x-state-refreshing state) t)
      (unwind-protect (xclhb:set-keycode-keysym-table (x-state-client state))
        (setf (x-state-refreshing state) nil))
      (let ((pending (nreverse (x-state-pending-keys state))))
        (setf (x-state-pending-keys state) nil)
        (dolist (event pending) (dispatch-key display event))))))

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
    (xclhb:set-event-handler c 2 (lambda (e) (dispatch-key d e)))
    (xclhb:set-event-handler c 12
                             (lambda (e)
                               (declare (ignore e))
                               (setf (display-damage d)
                                       (list
                                        (list 0 0 (display-width d) (display-height d))))))
    (xclhb:set-event-handler c 22
                             (lambda (e)
                               (let ((w (xclhb:configure-notify-event-width e))
                                     (h (xclhb:configure-notify-event-height e)))
                                 ;; The generated event struct drops the sequence field (pad at
                                 ;; bytes 2-3). XCLHB:PROCESS-INPUT-ONE reads each 32-byte event
                                 ;; into the client input buffer at offset 0 and calls this
                                 ;; handler before reading again, so bytes 2-3 hold this event's
                                 ;; sequence. Diagnostic only; never used for causality.
                                 (setf (x-state-configure-sequence (display-backend-state d))
                                       (xclhb::read-card16 (xclhb:client-input-buffer c) 2))
                                 (unless (and (= w (display-width d)) (= h (display-height d)))
                                   (resize-display d w h)
                                   (queue-event d (make-resize-event
                                                   :width w :height h
                                                   :columns (display-columns d) :rows (display-rows d)
                                                   :serial (x-state-configure-sequence (display-backend-state d)))))
                                  (x11-geometry-configured d w h))))
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
  (let ((r (lispterm.display:display-geometry-request display)))
    (when (and (lispterm.display:geometry-active-p display)
               (x11-target-matches-p r width height))
      (lispterm.display:geometry-result display r
                                       :surface-ready)
      (setf (display-damage display) (list (list 0 0 width height))))))
(defun x11-start-size-request (display)
  (let ((s (display-backend-state display)))
    (when (and (x-state-geometry-next s) (not (x-state-geometry-query-p s)))
      (let ((request (x-state-geometry-next s)) (c (x-state-client s)))
        (setf (x-state-geometry-next s) nil (x-state-geometry-query-p s) t)
        ;; Constants denote bit positions; bitcase takes every positional field.
        (unless (x11-target-matches-p request (display-width display) (display-height display))
          (xclhb:configure-window c (x-state-window s)
                                 (logior (ash 1 xclhb:+config-window--width+)
                                         (ash 1 xclhb:+config-window--height+))
                                 0 0 (lispterm.display:geometry-request-width request)
                                 (lispterm.display:geometry-request-height request) 0 0 0))
        ;; At most one outstanding query and one coalesced target, even if a
        ;; peer stops replying while the child repeatedly issues DECCOLM.
        (xclhb:get-geometry c
          (lambda (reply)
            (setf (x-state-geometry-query-p s) nil)
            (when (and (lispterm.display:geometry-active-p display)
                       (lispterm.display:geometry-current-p display request))
              (let ((w (xclhb:get-geometry-reply-width reply))
                    (h (xclhb:get-geometry-reply-height reply)))
                (when (x11-target-matches-p request w h)
                  (unless (and (= w (display-width display)) (= h (display-height display)))
                    (queue-event display (resize-display display w h)))
                  (x11-geometry-configured display w h))))
            (x11-start-size-request display))
          (x-state-window s))
        (xclhb:flush c)))))
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
          (when (and (lispterm.display:geometry-active-p display)
                     (lispterm.display:geometry-current-p display r)
                     (x11-target-matches-p r (xclhb:get-geometry-reply-width reply)
                                           (xclhb:get-geometry-reply-height reply))
                     (x11-target-matches-p r (display-width display) (display-height display)))
            (incf (lispterm.display:display-presentation-generation display))
            (lispterm.display:geometry-result display r :settled :server-compatible))
          (x11-start-size-request display))
        (x-state-window s)))))
