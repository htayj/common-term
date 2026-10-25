(in-package #:lispterm.display)

(defun keysym-keypad-key (keysym)
  "Keep X11/XKB keypad identity, including the Num Lock navigation aliases."
  (or (when (<= #xffb0 keysym #xffb9)
        (nth (- keysym #xffb0) '(:kp0 :kp1 :kp2 :kp3 :kp4 :kp5 :kp6 :kp7 :kp8 :kp9)))
      (cdr (assoc keysym '((#xff8d . :kp-enter)
                          (#xff91 . :pf1) (#xff92 . :pf2) (#xff93 . :pf3) (#xff94 . :pf4)
                          (#xff95 . :kp7) (#xff96 . :kp4) (#xff97 . :kp8)
                          (#xff98 . :kp6) (#xff99 . :kp2) (#xff9a . :kp9)
                          (#xff9b . :kp3) (#xff9c . :kp1) (#xff9d . :kp5)
                          (#xff9e . :kp0) (#xff9f . :kp-period)
                          (#xffac . :kp-comma) (#xffad . :kp-minus) (#xffae . :kp-period))))))

(defun keypad-definition (key)
  ;; vttest 20251205 keyboard.c num_keypadtab: numeric text, application suffix.
  (assoc key '((:kp0 "0" "p") (:kp1 "1" "q") (:kp2 "2" "r")
               (:kp3 "3" "s") (:kp4 "4" "t") (:kp5 "5" "u")
               (:kp6 "6" "v") (:kp7 "7" "w") (:kp8 "8" "x")
               (:kp9 "9" "y") (:kp-minus "-" "m") (:kp-comma "," "l")
               (:kp-period "." "n") (:kp-enter #.(string #\Return) "M"))))

(defun keypad-text (key)
  (second (keypad-definition key)))

(defun key-event-bytes (event &key application-cursor application-keypad vt52)
  "Encode a normalized key in ANSI or VT52 cursor/keypad mode."
  (let* ((key (key-event-key event))
         (mods (key-event-modifiers event))
         (modifier (+ 1 (if (member :shift mods) 1 0)
                        (if (member :alt mods) 2 0)
                        (if (member :control mods) 4 0)))
         (arrow (cdr (assoc key '((:up . "A") (:down . "B") (:right . "C")
                                  (:left . "D") (:home . "H") (:end . "F")))))
         (function-index (or (position key '(:pf1 :pf2 :pf3 :pf4))
                             (position key '(:f1 :f2 :f3 :f4))))
         (keypad (keypad-definition key))
         (tilde-index (position key '(:insert :delete :prior :next :f5 :f6
                                     :f7 :f8 :f9 :f10 :f11 :f12)))
         (body
           (cond
             (keypad (if application-keypad
                         (format nil "~c~a~a" #\Esc (if vt52 "?" "O") (third keypad))
                         (second keypad)))
             ((and vt52 (member key '(:up :down :right :left)))
              (format nil "~c~a" #\Esc arrow))
             (arrow (if (= modifier 1)
                        (format nil "~c~a~a" #\Esc (if application-cursor "O" "[") arrow)
                        (format nil "~c[1;~d~a" #\Esc modifier arrow)))
             (function-index
              (let ((letter (code-char (+ 80 function-index))))
                (cond (vt52 (format nil "~c~c" #\Esc letter))
                      ((= modifier 1) (format nil "~cO~c" #\Esc letter))
                      (t (format nil "~c[1;~d~c" #\Esc modifier letter)))))
             (tilde-index
              (let ((number (nth tilde-index '(2 3 5 6 15 17 18 19 20 21 23 24))))
                (if (= modifier 1) (format nil "~c[~d~~" #\Esc number)
                    (format nil "~c[~d;~d~~" #\Esc number modifier))))
             ((eq key :return) (string #\Return))
             ((eq key :backspace) (string (code-char 127)))
             ((eq key :escape) (string #\Esc))
             ((eq key :tab) (if (member :shift mods) (format nil "~c[Z" #\Esc) (string #\Tab)))
             ((and (characterp key) (member :control mods))
              (string (code-char (if (char= key #\?) 127
                                    (logand 31 (char-code (char-upcase key)))))))
             ((key-event-text event))
             ((characterp key) (string key))
             (t ""))))
    (babel:string-to-octets
     (if (and (member :alt mods) (not (or arrow function-index tilde-index
                                         (and keypad application-keypad))))
         (concatenate 'string (string #\Esc) body) body)
     :encoding :utf-8)))
