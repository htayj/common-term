(in-package #:nerimux/terminal/parser)

;; VT100 User Guide 3.3.5 and xterm VTPrsTbl.c/charproc.c VT52 emulation.
;; Keep VT52 separate: ESC [ is not CSI, ESC D is left, ESC H is home.
;; ANSI modes/charset designations survive ESC <. DEC STD 070 leaves most
;; cross-mode state undefined; we preserve it, but VT52 text never wraps.
(defun vt52-control (screen byte)
  "Execute the supported VT52 C0 controls. Other C0 and DEL are ignored."
  (case byte
    (#x07 (set-bell-pending screen)
           (screen-record-notification screen (vector #x07) ""))
    (#x08 (cursor-bs screen))
    (#x09 (cursor-ht screen))
    ((#x0a #x0b #x0c) (cursor-lf screen))
    (#x0d (cursor-cr screen))))

(defun vt52-ground-state (screen byte)
  (let ((byte (logand byte #x7f)))
    (cond ((= byte #x1b) #'vt52-escape-state)
          ((<= #x20 byte #x7e)
           (write-char-at-cursor screen (code-char byte))
           (setf (screen-pending-wrap screen) nil)
           #'ground-state)
          (t (vt52-control screen byte) #'ground-state))))

(defun make-vt52-address-k (&optional row)
  "Collect ESC Y's two offset-32 coordinates across reads.
C0 controls execute without becoming coordinates; CAN/SUB cancel and ESC
restarts. DEL is ignored. As in VT100 emulation in xterm (DEC STD 070 A-28),
an oversized coordinate leaves that axis unchanged. Valid coordinates use
the shared physical positioning helper, independent of DECOM."
  (lambda (screen byte)
    (let ((byte (logand byte #x7f)))
      (cond ((= byte #x1b) #'vt52-escape-state)
            ((member byte '(#x18 #x1a)) #'ground-state)
            ((or (< byte #x20) (= byte #x7f))
             (vt52-control screen byte)
             (make-vt52-address-k row))
            ((null row) (make-vt52-address-k (- byte #x20)))
            (t (let* ((column (- byte #x20))
                      (y (if (< row (screen-height screen))
                             row (screen-cursor-y screen)))
                      (x (if (< column (screen-width screen))
                             column (screen-cursor-x screen))))
                 (set-cursor screen x y))
               #'ground-state)))))

(defun vt52-ignore-state (screen byte)
  "Discard an unsupported ESC intermediate sequence through its final byte."
  (let ((byte (logand byte #x7f)))
    (cond ((= byte #x1b) #'vt52-escape-state)
          ((member byte '(#x18 #x1a)) #'ground-state)
          ((or (< byte #x20) (= byte #x7f))
           (vt52-control screen byte) #'vt52-ignore-state)
          ((< byte #x30) #'vt52-ignore-state)
          (t #'ground-state))))

(defun vt52-escape-state (screen byte)
  (let ((byte (logand byte #x7f)))
    (cond
      ((= byte #x1b) #'vt52-escape-state)
      ((member byte '(#x18 #x1a)) #'ground-state)
      ((or (< byte #x20) (= byte #x7f))
       (vt52-control screen byte) #'vt52-escape-state)
      ((< byte #x30) #'vt52-ignore-state)
      (t
       (case byte
         (#x41 (cursor-up screen 1))
         (#x42 (cursor-down screen 1))
         (#x43 (cursor-right screen 1))
         (#x44 (cursor-left screen 1))
         (#x46 (setf (screen-vt52-graphics screen) t))
         (#x47 (setf (screen-vt52-graphics screen) nil))
         (#x48 (set-cursor screen 0 0))
         (#x49 (cursor-ri screen))
         (#x4a (erase-display screen 0))
         (#x4b (erase-line screen 0))
         (#x59 (return-from vt52-escape-state (make-vt52-address-k)))
         (#x5a (push (format nil "~C/Z" #\Escape)
                     (screen-response-queue screen)))
         (#x3d (setf (screen-app-keypad screen) t))
         (#x3e (setf (screen-app-keypad screen) nil))
         (#x3c (setf (screen-vt52-mode screen) nil
                     (screen-vt52-graphics screen) nil
                     (screen-pending-wrap screen) nil)))
       #'ground-state))))
