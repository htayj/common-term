(in-package #:lispterm.display)

(defun key-event-bytes (event &key application-cursor)
  "Encode a normalized key using UTF-8 and xterm-style special-key sequences."
  (let* ((key (key-event-key event))
         (mods (key-event-modifiers event))
         (modifier (+ 1 (if (member :shift mods) 1 0)
                        (if (member :alt mods) 2 0)
                        (if (member :control mods) 4 0)))
         (arrow (cdr (assoc key '((:up . "A") (:down . "B") (:right . "C")
                                  (:left . "D") (:home . "H") (:end . "F")))))
         (function-index (position key '(:f1 :f2 :f3 :f4)))
         (tilde-index (position key '(:insert :delete :prior :next :f5 :f6
                                     :f7 :f8 :f9 :f10 :f11 :f12)))
         (body
           (cond
             (arrow (if (= modifier 1)
                        (format nil "~c~a~a" #\Esc (if application-cursor "O" "[") arrow)
                        (format nil "~c[1;~d~a" #\Esc modifier arrow)))
             (function-index
              (let ((letter (code-char (+ 80 function-index))))
                (if (= modifier 1) (format nil "~cO~c" #\Esc letter)
                    (format nil "~c[1;~d~c" #\Esc modifier letter))))
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
     (if (and (member :alt mods) (not (or arrow function-index tilde-index)))
         (concatenate 'string (string #\Esc) body) body)
     :encoding :utf-8)))
