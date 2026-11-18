(in-package #:lispterm.display.wayland)

(defstruct wayland-state
  wire registry compositor shm wm seat keyboard surface xdg-surface toplevel
  (outputs (make-hash-table)) (entered-outputs nil) (scale 1)
  (width 800) (height 480) pending-width pending-height
  pending-states states configure-serial (configure-generation 0)
  committed-width committed-height committed-buffer-width committed-buffer-height
  committed-request-id committed-epoch frame-request-id frame-epoch
  (commit-generation 0) (frame-generation 0)
  geometry-request geometry-next geometry-phase geometry-display
  (normal-min-width 0) (normal-min-height 0) (normal-max-width 0) (normal-max-height 0)
  (configured-p nil) (closed-p nil) (frame-pending-p nil)
  (buffers nil) (generation 0) (event-sink (constantly nil))
  (resize-sink (constantly nil)) (redraw-p t) xkb
  (repeat-rate 25) (repeat-delay 600) repeat-key repeat-at
  (title "common-term") (app-id "org.autolith.common-term"))
(defun protocol-pump (state &optional (timeout 0))
  (let ((wire (wayland-state-wire state)))
    (wire-poll wire timeout) (wire-dispatch wire)))
(defun protocol-roundtrip (state &optional (timeout 5))
  (let* ((wire (wayland-state-wire state)) (done nil)
         (id (wire-id wire :callback (lambda (id opcode payload)
                                      (declare (ignore opcode payload))
                                      (remhash id (wire-handlers wire)) (setf done t))))
         (end (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (wire-send wire 1 0 id)
    (loop until done do
      (when (> (get-internal-real-time) end) (wire-error "roundtrip timeout" nil))
      (protocol-pump state .05))))
(defun protocol-emit (state type &rest values)
  (funcall (wayland-state-event-sink state) (cons type values)))
(defun protocol-scale (state)
  (let ((scale (loop for id in (wayland-state-entered-outputs state)
                     maximize (getf (gethash id (wayland-state-outputs state)) :scale 1) into n
                     finally (return (max 1 (or n 1))))))
    (unless (= scale (wayland-state-scale state))
      (setf (wayland-state-scale state) scale (wayland-state-redraw-p state) t)
      (incf (wayland-state-generation state))
      (funcall (wayland-state-resize-sink state)
               (wayland-state-width state) (wayland-state-height state) scale))))
(defun registry-bind (state name interface version handler)
  (let* ((wire (wayland-state-wire state)) (id (wire-id wire interface handler)))
    (wire-send wire (wayland-state-registry state) 0 name (wl-string interface) version id) id))
(defvar *protocol-global-hooks* nil)
(defun bind-global (state name interface version)
  (let ((wire (wayland-state-wire state)))
    (dolist (hook *protocol-global-hooks*) (funcall hook state name interface version))
    (cond
      ((and (string= interface "wl_compositor") (not (wayland-state-compositor state)))
       (when (< version 4) (wire-error "wl_compositor version 4 required" nil))
       (setf (wayland-state-compositor state)
             (registry-bind state name interface 4 (constantly nil))))
      ((and (string= interface "wl_shm") (not (wayland-state-shm state)))
       (setf (wayland-state-shm state)
             (registry-bind state name interface 1 (constantly nil))))
      ((and (string= interface "xdg_wm_base") (not (wayland-state-wm state)))
       (setf (wayland-state-wm state)
             (registry-bind state name interface 1
               (lambda (id opcode payload)
                 (when (= opcode 0) (wire-send wire id 3 (bytes-u32 payload 0)))))))
      ((and (string= interface "wl_seat") (not (wayland-state-seat state)))
       (setf (wayland-state-seat state)
             (registry-bind state name interface (min 5 version)
               (lambda (id opcode payload)
                 (when (and (= opcode 0) (logbitp 1 (bytes-u32 payload 0))
                            (not (wayland-state-keyboard state)))
                   (let ((keyboard (wire-id wire :keyboard
                                            (lambda (id opcode payload)
                                              (declare (ignore id))
                                              (keyboard-event state opcode payload)))))
                     (setf (wayland-state-keyboard state) keyboard)
                     (wire-send wire id 1 keyboard)))))))
      ((string= interface "wl_output")
       (let ((id (registry-bind state name interface (min 2 version)
                   (lambda (id opcode payload)
                     (when (= opcode 3)
                       (setf (getf (gethash id (wayland-state-outputs state)) :scale)
                             (max 1 (signed32 (bytes-u32 payload 0)))))
                     (when (= opcode 2) (protocol-scale state))))))
         (setf (gethash id (wayland-state-outputs state)) (list :global name :scale 1)))))))
(defun protocol-open (&key name (width 800) (height 480) (title "common-term")
                           (event-sink (constantly nil)) (resize-sink (constantly nil)))
  (let* ((wire (wire-connect name))
         (state (make-wayland-state :wire wire :width width :height height
                                   :title title :event-sink event-sink :resize-sink resize-sink)))
    (handler-case
        (progn
          (setf (gethash 1 (wire-handlers wire))
                (lambda (id opcode payload)
                  (declare (ignore id))
                  (case opcode
                    (0 (wire-error (format nil "server error on ~D code ~D: ~A"
                                           (bytes-u32 payload 0) (bytes-u32 payload 4)
                                           (payload-string payload 8)) nil))
                    (1 (let ((deleted (bytes-u32 payload 0)))
                         (remhash deleted (wire-handlers wire))
                         (remhash deleted (wire-interfaces wire)))))))
          (setf (wayland-state-registry state)
                (wire-id wire :registry
                  (lambda (id opcode payload)
                    (declare (ignore id))
                    (case opcode
                      (0 (multiple-value-bind (interface offset) (payload-string payload 4)
                           (bind-global state (bytes-u32 payload 0) interface
                                              (bytes-u32 payload offset))))
                      (1 ; Global removal: leave outputs bound until connection close, but stop scaling by them.
                       (let ((global (bytes-u32 payload 0)))
                         (maphash (lambda (id info)
                                    (when (= global (getf info :global))
                                      (setf (wayland-state-entered-outputs state)
                                            (remove id (wayland-state-entered-outputs state)))))
                                  (wayland-state-outputs state))
                         (protocol-scale state)))))))
          (wire-send wire 1 1 (wayland-state-registry state))
          (protocol-roundtrip state)
          (unless (and (wayland-state-compositor state) (wayland-state-shm state)
                       (wayland-state-wm state))
            (wire-error "required wl_compositor/wl_shm/xdg_wm_base globals absent" nil))
          (setf (wayland-state-surface state)
                (wire-id wire :surface
                  (lambda (id opcode payload)
                    (declare (ignore id))
                    (case opcode
                      (0 (pushnew (bytes-u32 payload 0) (wayland-state-entered-outputs state)))
                      (1 (setf (wayland-state-entered-outputs state)
                               (remove (bytes-u32 payload 0) (wayland-state-entered-outputs state)))))
                    (protocol-scale state))))
          (wire-send wire (wayland-state-compositor state) 0 (wayland-state-surface state))
          (setf (wayland-state-xdg-surface state)
                (wire-id wire :xdg-surface
                  (lambda (id opcode payload)
                    (when (= opcode 0)
                      (wire-send wire id 4 (bytes-u32 payload 0)) ; ACK before attach
                      (protocol-configure state (bytes-u32 payload 0))))))
          (wire-send wire (wayland-state-wm state) 2 (wayland-state-xdg-surface state)
                     (wayland-state-surface state))
          (setf (wayland-state-toplevel state)
                (wire-id wire :xdg-toplevel
                  (lambda (id opcode payload)
                    (declare (ignore id))
                    (case opcode
                      (0 (let ((w (signed32 (bytes-u32 payload 0)))
                               (h (signed32 (bytes-u32 payload 4))))
                           (when (or (minusp w) (minusp h) (> w 16384) (> h 16384)
                                     (> (* (max 1 w) (max 1 h)) 67108864))
                             (wire-error "unsafe configure dimensions" nil))
                           ;; Keep zero dimensions until the terminating serial.
                           (let ((length (bytes-u32 payload 8)))
                             (unless (and (zerop (mod length 4)) (= (+ 12 length) (length payload)))
                               (wire-error "invalid toplevel state array" nil))
                             (setf (wayland-state-pending-states state)
                                   (loop for offset from 12 below (+ 12 length) by 4
                                         collect (bytes-u32 payload offset))))
                           (setf (wayland-state-pending-width state) w
                                 (wayland-state-pending-height state) h)))
                      (1 (setf (wayland-state-closed-p state) t)
                         (protocol-emit state :close))))))
          (wire-send wire (wayland-state-xdg-surface state) 1 (wayland-state-toplevel state))
          (wire-send wire (wayland-state-toplevel state) 2 (wl-string title))
          (wire-send wire (wayland-state-toplevel state) 3 (wl-string (wayland-state-app-id state)))
          ;; An empty commit requests the initial configure; no buffer may precede its ACK.
          (wire-send wire (wayland-state-surface state) 6)
          (loop with end = (+ (get-internal-real-time) (* 5 internal-time-units-per-second))
                until (wayland-state-configured-p state) do
            (when (> (get-internal-real-time) end) (wire-error "initial configure timeout" nil))
            (protocol-pump state .05))
          (protocol-roundtrip state) state)
      (error (c) (protocol-close state) (error c)))))

(defun protocol-size (state width height)
  (unless (and (= width (wayland-state-width state)) (= height (wayland-state-height state)))
    (setf (wayland-state-width state) width (wayland-state-height state) height)
    (incf (wayland-state-generation state)))
  (setf (wayland-state-redraw-p state) t)
  (funcall (wayland-state-resize-sink state) width height (wayland-state-scale state)))
(defun protocol-configure (state serial)
  (let* ((r (or (wayland-state-geometry-next state) (wayland-state-geometry-request state)))
         (w (wayland-state-pending-width state)) (h (wayland-state-pending-height state))
         (states (copy-list (wayland-state-pending-states state))))
    (setf (wayland-state-configure-serial state) serial
          (wayland-state-states state) states
          (wayland-state-configured-p state) t)
    (incf (wayland-state-configure-generation state))
    (protocol-size state
                   (if (and w (plusp w)) w
                       (if r (lispterm.display:geometry-request-width r) (wayland-state-width state)))
                   (if (and h (plusp h)) h
                       (if r (lispterm.display:geometry-request-height r) (wayland-state-height state))))
    (protocol-emit state :resize
                   :width (wayland-state-width state) :height (wayland-state-height state)
                   :serial serial :states states :scale (wayland-state-scale state)
                   :interactive-p (not (null (member 3 states))))
    (when (and (wayland-state-geometry-request state) (intersection states '(1 2 3)))
      (let ((display (wayland-state-geometry-display state)))
        (lispterm.display:cancel-size-request display
          (if (member 3 states) :cancelled :refused))))))

(defun protocol-geometry-hints (state min-w min-h max-w max-h)
  ;; Clear the old pair before installing conflicting dimensions: min <= max
  ;; must hold for every intermediate request, not only the final commit.
  (let ((wire (wayland-state-wire state)) (top (wayland-state-toplevel state)))
    (wire-send wire top 8 0 0)
    (wire-send wire top 7 0 0)
    (wire-send wire top 7 max-w max-h)
    (wire-send wire top 8 min-w min-h)))
(defun protocol-geometry-close (state &optional settled-p)
  "Release constraints on every exit and close the transaction behind a sync fence."
  (when (and (wayland-state-geometry-request state)
             (not (eq :closing (wayland-state-geometry-phase state))))
    (let* ((r (wayland-state-geometry-request state))
           (display (wayland-state-geometry-display state))
           (wire (wayland-state-wire state)))
      (setf (wayland-state-geometry-phase state) :closing)
      (protocol-geometry-hints state
        (wayland-state-normal-min-width state) (wayland-state-normal-min-height state)
        (wayland-state-normal-max-width state) (wayland-state-normal-max-height state))
      (wire-send wire (wayland-state-surface state) 6)
      (let ((callback (wire-id wire :callback
                        (lambda (id opcode payload)
                          (declare (ignore opcode payload))
                          (remhash id (wire-handlers wire))
                          (when (eq r (wayland-state-geometry-request state))
                            (setf (wayland-state-geometry-request state) nil
                                  (wayland-state-geometry-phase state) nil)
                            (when (and settled-p (lispterm.display:geometry-active-p display))
                              (let ((compatible
                                      (and (= (wayland-state-width state)
                                              (lispterm.display:geometry-request-width r))
                                           (= (wayland-state-height state)
                                              (lispterm.display:geometry-request-height r))
                                           (not (intersection (wayland-state-states state) '(1 2 3))))))
                                (lispterm.display:geometry-result display r
                                  (if compatible :settled :refused)
                                  (if compatible :surface-compatible :configure-after-frame))))
                            (protocol-geometry-start state))))))
        (wire-send wire 1 0 callback)))))
(defun protocol-geometry-start (state)
  (when (and (not (wayland-state-geometry-request state))
             (wayland-state-geometry-next state))
    (let* ((r (wayland-state-geometry-next state))
           (display (wayland-state-geometry-display state)))
      (setf (wayland-state-geometry-next state) nil)
      (if (intersection (wayland-state-states state) '(1 2 3))
          (lispterm.display:geometry-result display r
                                           :refused :constrained-toplevel)
          (let ((w (lispterm.display:geometry-request-width r))
                (h (lispterm.display:geometry-request-height r)))
            (setf (wayland-state-geometry-request state) r
                  (wayland-state-geometry-phase state) :await-frame)
            (protocol-geometry-hints state w h w h)
            ;; A floating client may choose a new surface size without waiting
            ;; for a configure that the compositor is not required to send.
            (protocol-size state w h)
            (wire-send (wayland-state-wire state) (wayland-state-xdg-surface state) 3 0 0 w h)
            (wire-send (wayland-state-wire state) (wayland-state-surface state) 6)
            (lispterm.display:geometry-result display r
                                             :surface-ready))))))
(defun protocol-geometry-frame (state request width height)
  (when (and request
             (lispterm.display:geometry-current-p (wayland-state-geometry-display state) request)
             (eq request (wayland-state-geometry-request state))
             (eq :await-frame (wayland-state-geometry-phase state)))
    (let* ((display (wayland-state-geometry-display state))
           (compatible (and (= width (lispterm.display:geometry-request-width request))
                            (= height (lispterm.display:geometry-request-height request))
                            (= width (wayland-state-width state))
                            (= height (wayland-state-height state))
                            (not (intersection (wayland-state-states state) '(1 2 3))))))
      (when compatible
        (incf (lispterm.display:display-presentation-generation display))
        (lispterm.display:geometry-result display request :closing))
      (protocol-geometry-close state compatible))))
