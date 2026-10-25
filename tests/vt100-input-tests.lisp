;;;; Deterministic VT100/VT52 input tests, no display server or GUI.
(defpackage #:lispterm.vt100-input.tests
  (:use #:cl) (:export #:run-tests)
  (:local-nicknames (#:wl #:lispterm.display.wayland)))
(in-package #:lispterm.vt100-input.tests)
(defvar *checks* 0)
(defun check (label value)
  (incf *checks*)
  (unless value (error "VT100 input check failed: ~S" label)))
(defun bytes= (expected key &rest options)
  (equalp (if (stringp expected) (babel:string-to-octets expected :encoding :utf-8) expected)
          (apply #'ltd:key-event-bytes (ltd:make-key-event :key key) options)))

;; Independently transcribed from vttest 20251205 keyboard.c:263-285,
;; num_keypadtab, and :195-203 VT100_curkeytab. No planner-derived constants.
(defparameter *keypad-cases*
  '((:kp0 #xffb0 "0" #\p) (:kp1 #xffb1 "1" #\q) (:kp2 #xffb2 "2" #\r)
    (:kp3 #xffb3 "3" #\s) (:kp4 #xffb4 "4" #\t) (:kp5 #xffb5 "5" #\u)
    (:kp6 #xffb6 "6" #\v) (:kp7 #xffb7 "7" #\w) (:kp8 #xffb8 "8" #\x)
    (:kp9 #xffb9 "9" #\y) (:kp-minus #xffad "-" #\m) (:kp-comma #xffac "," #\l)
    (:kp-period #xffae "." #\n) (:kp-enter #xff8d #.(string #\Return) #\M)))
(defparameter *aliases*
  '((#xff95 :kp7) (#xff96 :kp4) (#xff97 :kp8) (#xff98 :kp6)
    (#xff99 :kp2) (#xff9a :kp9) (#xff9b :kp3) (#xff9c :kp1)
    (#xff9d :kp5) (#xff9e :kp0) (#xff9f :kp-period)
    (#xff91 :pf1) (#xff92 :pf2) (#xff93 :pf3) (#xff94 :pf4)))

(defun encoding-tests ()
  (dolist (case *keypad-cases*)
    (destructuring-bind (key sym numeric suffix) case
      (declare (ignore sym))
      (dolist (vt52 '(nil t))
        (check (list key :numeric vt52) (bytes= numeric key :vt52 vt52))
        (check (list key :application vt52)
               (bytes= (format nil "~c~a~c" #\Esc (if vt52 "?" "O") suffix)
                       key :application-keypad t :vt52 vt52)))))
  (loop for key in '(:pf1 :pf2 :pf3 :pf4 :f1 :f2 :f3 :f4)
        for suffix in '(#\P #\Q #\R #\S #\P #\Q #\R #\S)
        do (dolist (vt52 '(nil t))
             (dolist (application '(nil t))
               (check (list key vt52 application)
                      (bytes= (format nil "~c~a~c" #\Esc (if vt52 "" "O") suffix)
                              key :application-keypad application :vt52 vt52)))))
  (loop for key in '(:up :down :right :left) for suffix across "ABCD"
        do (dolist (vt52 '(nil t))
             (dolist (application '(nil t))
               (check (list key vt52 application)
                      (bytes= (format nil "~c~a~c" #\Esc
                                      (cond (vt52 "") (application "O") (t "[")) suffix)
                              key :application-cursor application :vt52 vt52)))))
  (dolist (case '((#\c (:control :alt) (27 3))
                  (#\? (:control) (127)) (#\λ nil (206 187))
                  (:up (:shift :control) (27 91 49 59 54 65))
                  (:tab (:shift) (27 91 90)) (:return nil (13))
                  (:backspace nil (127)) (:escape nil (27))
                  (:f5 (:alt) (27 91 49 53 59 51 126))
                  (:kp1 (:alt) (27 49))))
    (destructuring-bind (key modifiers expected) case
      (check case (equalp (coerce expected 'vector)
                         (ltd:key-event-bytes (ltd:make-key-event :key key :modifiers modifiers))))))
  (check :multibyte-text
         (equalp #(206 187 195 169)
                 (ltd:key-event-bytes (ltd:make-key-event :key #\a :text "λé"))))
  (check :application-keypad-alt-not-double-escaped
         (equalp #(27 79 113)
                 (ltd:key-event-bytes (ltd:make-key-event :key :kp1 :modifiers '(:alt))
                                     :application-keypad t))))

(defun x11-normalization-tests ()
  (dolist (case (append (mapcar (lambda (c) (list (second c) (first c))) *keypad-cases*)
                       *aliases*))
    (destructuring-bind (sym key) case
      (check (list :x11 sym) (eq key (lispterm.display.x11::translate-keysyms (list sym) 0)))
      (with-open-stream (stream (make-string-input-stream ""))
        (let ((client (xclhb:make-client :stream stream :keycode-keysym-table (list (list 10 sym)))))
          (check (list :x11-client-mapping sym)
                 (eq key (lispterm.display.x11::translate-key client 10 0)))))))
  ;; Real X keymaps commonly pair navigation and numeric keypad symbols.
  (dolist (pair '((#xff9c #xffb1 :kp1) (#xff99 #xffb2 :kp2) (#xff9f #xffae :kp-period)))
    (destructuring-bind (normal shifted key) pair
      (dolist (mask '(0 1 16 17))
        (check (list :x11-levels pair mask)
               (eq key (lispterm.display.x11::translate-keysyms (list normal shifted) mask))))))
  (dolist (case '(((97 65) 0 #\a) ((97 65) 1 #\A) ((97 65) 2 #\A)
                  ((97 65) 3 #\a) ((#x10003bb) 0 #\λ) ((#xff52) 0 :up)))
    (destructuring-bind (symbols mask expected) case
      (check case (eql expected (lispterm.display.x11::translate-keysyms symbols mask))))))

(defun xkb-normalization-tests ()
  "Use a real libxkbcommon keymap with every received keysym, not FFI stubs."
  (wl::ensure-xkb-library)
  (let* ((cases (append (mapcar (lambda (c) (list (second c) (first c))) *keypad-cases*)
                        *aliases*))
         (text (with-output-to-string (out)
                 (format out "xkb_keymap { xkb_keycodes \"test\" { minimum=8; maximum=100; ")
                 (loop for i from 0 below (length cases)
                       do (format out "<K~3,'0D>=~D; " i (+ i 8)))
                 (format out "}; xkb_types \"test\" { type \"ONE_LEVEL\" { modifiers=None; map[None]=Level1; }; }; xkb_compatibility \"test\" {}; xkb_symbols \"test\" { ")
                 (loop for (sym key) in cases for i from 0
                       do (format out "key <K~3,'0D> { [ 0x~X ] }; " i sym))
                 (format out "}; };")))
         (xkb (wl::make-xkb)))
    (unwind-protect
         (progn
           (setf (wl::xkb-context xkb) (wl::%xkb-context-new 0))
           (check :xkb-context (not (cffi:null-pointer-p (wl::xkb-context xkb))))
           (cffi:with-foreign-string (buffer text :encoding :utf-8)
             (setf (wl::xkb-keymap xkb)
                   (wl::%xkb-keymap-new (wl::xkb-context xkb) buffer (length text) 1 0)))
           (check :xkb-keymap (not (cffi:null-pointer-p (wl::xkb-keymap xkb))))
           (setf (wl::xkb-state xkb) (wl::%xkb-state-new (wl::xkb-keymap xkb)))
           (check :xkb-state (not (cffi:null-pointer-p (wl::xkb-state xkb))))
           (loop for (sym key) in cases for i from 0
                 for event = (wl::xkb-normalized-key xkb i)
                 do (check (list :xkb-actual-keysym sym)
                           (= sym (wl::%xkb-key-sym (wl::xkb-state xkb) (+ i 8))))
                    (check (list :xkb-key sym) (eq key (getf event :key)))
                    (when (assoc key *keypad-cases*)
                      (check (list :xkb-numeric-text sym)
                             (equal (third (assoc key *keypad-cases*)) (getf event :text))))))
      (wl::xkb-close xkb))))

(defun run-tests ()
  (setf *checks* 0)
  (encoding-tests)
  (x11-normalization-tests)
  (xkb-normalization-tests)
  (format t "~&(:vt100-input-checks ~D :passed T)~%" *checks*))
