(require :asdf)
(load "scripts/bootstrap.lisp")
(asdf:load-system :cffi)
(asdf:load-system :babel)
(dolist (file '("wayland-wire" "wayland-protocol" "wayland-shm" "xkb"))
  (load (format nil "src/display/~A.lisp" file)))
(let ((state nil))
  (unwind-protect
      (progn
        (setf state (lispterm.display.wayland::protocol-open
                     :title "Lispterm native Wayland spike"
                     :event-sink (lambda (event) (format t "EVENT ~S~%" event))))
        (let* ((width (lispterm.display.wayland::wayland-state-width state))
               (height (lispterm.display.wayland::wayland-state-height state))
               (pixels (make-array (* width height) :element-type '(unsigned-byte 32)
                                                     :initial-element #xff224466)))
          (assert (lispterm.display.wayland::protocol-present state pixels width height))
          (dotimes (i 100) (lispterm.display.wayland::protocol-pump state .05))
          (format t "NATIVE-WAYLAND-SPIKE PASS configured=~S keymap=~S frame=~S~%"
                  (lispterm.display.wayland::wayland-state-configured-p state)
                  (not (null (lispterm.display.wayland::wayland-state-xkb state)))
                  (not (lispterm.display.wayland::wayland-state-frame-pending-p state)))))
    (lispterm.display.wayland::protocol-close state)))
