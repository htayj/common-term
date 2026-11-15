;;;; Production scheduling regression controls; device boundaries simulated.
(in-package #:lispterm.owner.tests)
(let* ((checks 0) (failures 0) (polls 0) (attempts 0) (submitted 0) (closed 0)
       (now 0) (blocked t) (written nil)
       (lispterm::*input-queue-limit* 4)
       (state (lispterm.display.wayland::make-wayland-state :frame-pending-p t))
       (window (lispterm.display.wayland::make-wayland-window :protocol state :pending-frame-p t))
       (display (ltd::make-display :backend :wayland :backend-state window
                                  :open-p t :owner sb-thread:*current-thread* :grid :test))
       (session (lispterm::make-session nil 2 8))
       (host (lispterm::%make-host :session session :display display :state :attached))
       (schedule (lispterm::make-owner-schedule :next-frame internal-time-units-per-second))
       (lispterm::*owner-clock* (lambda () now))
       (lispterm.display.wayland::*direct-present-hook*
         (lambda (d)
           (declare (ignore d)) (incf attempts)
           (if (lispterm.display.wayland::wayland-state-frame-pending-p state) :busy
               (progn (incf submitted) :presented)))))
  (setf (lispterm::session-pty session) (lispterm.pty::%make-pty :open-p nil))
  (labels ((verify (name value)
             (incf checks) (unless value (incf failures))
             (format t "~S~%" (list :check name :pass (not (null value)))))
           (tick () (lispterm::owner-iteration host schedule)))
    (with-functions
     (list
      (cons 'lispterm.display.wayland::protocol-pump
            (lambda (s &optional timeout)
              (declare (ignore timeout)) (incf polls)
              (case polls
                (1 (push '(:key :key #\q) (lispterm.display.wayland::wayland-window-events window)))
                (2 (setf (lispterm.display.wayland::wayland-state-frame-pending-p s) nil))
                (3 (push '(:close) (lispterm.display.wayland::wayland-window-events window))))))
      (cons 'lispterm.display.wayland::keyboard-repeat (lambda (&rest x) (declare (ignore x))))
      (cons 'lispterm.display.wayland::protocol-collect-buffers (lambda (&rest x) (declare (ignore x))))
      (cons 'lispterm.pty:pty-write
            (lambda (pty bytes &key timeout)
              (declare (ignore pty timeout))
              (if blocked 0 (progn (loop for b across bytes do (push b written)) (length bytes)))))
      (cons 'lispterm.pty:pty-read (lambda (&rest x) (declare (ignore x)) nil))
      (cons 'lispterm.pty:pty-process (lambda (&rest x) (declare (ignore x)) nil))
      (cons 'ltd:close-display
            (lambda (d) (incf closed) (setf (ltd::display-open-p d) nil)))
      (cons 'lispterm::window-geometry-intent (lambda (&rest x) (declare (ignore x))))
      (cons 'ltd:geometry-tick (lambda (&rest x) (declare (ignore x))))
      (cons 'ltd:geometry-status (lambda (&rest x) (declare (ignore x))))
      (cons 'ltd:display-metrics (lambda (&rest x) (declare (ignore x))))
      (cons 'lispterm::render-session (lambda (&rest x) (declare (ignore x)))))
     (lambda ()
       (lispterm::session-write session (lispterm::utf8 "ABCD"))
       (let ((wait (tick)))
         (verify :blocked-native-poll (= polls 1))
         (verify :blocked-positive-wait (and wait (plusp wait))))
       (verify :native-key-rejection-recorded
               (eq :input-backpressure (getf (first (lispterm:host-errors host)) :operation)))
       (verify :blocked-keeps-attached (eq :attached (lispterm::host-state-value host)))
       (setf now 1) (tick)
       (verify :blocked-callback-pumped (= polls 2))
       (verify :blocked-callback-pending-frame-retried (= attempts 2))
       (verify :blocked-callback-submitted (= submitted 1))
       (verify :blocked-callback-cleared-pending
               (not (lispterm.display.wayland::wayland-window-pending-frame-p window)))
       (setf now 2) (tick)
       (verify :later-native-close-pumped (= polls 3))
       (verify :later-native-close-detached
               (and (= closed 1) (null (lispterm::host-display host))
                    (eq :detached (lispterm::host-state-value host))))
       (verify :accepted-bytes-survive-native-close (= 4 (lispterm::session-input-pending session)))
       (setf blocked nil now 3) (tick) (tick)
       (verify :unblock-exact-accepted-fifo-no-rejected-key
               (equalp (coerce (reverse written) 'vector) (lispterm::utf8 "ABCD")))
       (verify :detached-does-not-poll (= polls 3))
       (verify :queue-empty-after-recovery (zerop (lispterm::session-input-pending session)))
       (format t "~S~%" (list :checks checks :failures failures))
       (when (plusp failures) (uiop:quit 1))))))
