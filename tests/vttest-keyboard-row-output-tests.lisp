;;;; Focused controls for vttest-keyboard-row-output. Synthetic output text only:
;;;; never native, pixel, font, timing or acceptance proof. In literals ^ stands for ESC.
(load (merge-pathnames "vttest-keyboard-row-output.lisp" *load-truename*))
(defpackage #:vttest-keyboard-row-output-tests (:use #:cl))
(in-package #:vttest-keyboard-row-output-tests)

(defvar *failures* 0)
(defvar *checks* 0)
(defun x (s) (substitute (code-char 27) #\^ s))
(defun reasons (label id output &optional (start 0) (end (length output)))
  (vttest-keyboard-row-output:row-output-reasons label id output start end))
(defun check (name actual expected)
  (incf *checks*)
  (unless (equal actual expected)
    (incf *failures*)
    (format t "FAIL ~a: ~s, expected ~s~%" name actual expected)))

(defparameter *prompt* "Press each cursor key. Finish with TAB.")
(defparameter *up* (x "^[23;1H^[0K^[23;1H^[7m <27> [ A ^[m^[7m (Up arrow key) ^[m^[1;57HUP"))
(defparameter *down* (x "^[23;1H^[0K^[23;1H^[7m <27> [ B ^[m^[7m (Down arrow key) ^[m^[1;62HDN"))
(defparameter *tab* (x "^[23;1H^[0K^[23;1H^[7m <9> ^[m"))
(defparameter *mode* (x "^[?1h^[1;57H^[7mUP^[m^[21;1H<ANSI / Cursor key mode SET>"))
(defparameter *up1* (x "^[23;1H^[0K^[23;1H^[7m <27> O A ^[m^[7m (Up arrow key) ^[m^[1;57HUP"))
(defparameter *hold* (x "^[?1l^[23;1H^[0KPush <RETURN>"))
(defparameter *kp1* (x "^[23;1H^[0K^[23;1H^[7m 1 ^[m^[7m (Numeric 1 key) ^[m^[19;60H 1 "))

;;; Fresh proofs.
(check :first-key (reasons "keyboard-cursor" "keys0-up" (concatenate 'string *prompt* *up*)) nil)
(let ((o (concatenate 'string *up* *down*)))
  (check :second-key (reasons "keyboard-cursor" "keys0-down" o (length *up*)) nil)
  ;; Older interval: the previous key's redraw is still inside.
  (check :older-interval (reasons "keyboard-cursor" "keys0-down" o) '(:row-erase-count))
  ;; Interval shifted past this capture's target into the next key.
  (check :shifted-end (reasons "keyboard-cursor" "keys0-up" o) '(:wrong-redraw))
  ;; Interval shifted past the erase.
  (check :shifted-start (reasons "keyboard-cursor" "keys0-down" o (1+ (length *up*))) '(:no-row-erase)))
(check :another-key (reasons "keyboard-cursor" "keys0-up" *down*) '(:wrong-redraw))
(check :cut-before-redraw (reasons "keyboard-cursor" "keys0-up" *up* 0 (- (length *up*) 2))
       '(:cut-before-redraw))
(check :cut-before-name (reasons "keyboard-cursor" "keys0-up" *up* 0 30) '(:cut-before-redraw))
(check :later-row-output (reasons "keyboard-cursor" "keys0-up" (concatenate 'string *up* (x "^[7mX")))
       '(:later-row-output))
(check :keypad-key (reasons "keyboard-keypad" "keys0-kp_1" *kp1*) nil)
(check :keypad-wrong-case (reasons "keyboard-cursor" "keys0-up" *kp1*) '(:wrong-redraw))

;;; Phase change: the previous phase's TAB echo precedes the first key.
(check :after-tab (reasons "keyboard-cursor" "keys1-up" (concatenate 'string *tab* *mode* *up1*)) nil)
(check :tab-residue (reasons "keyboard-cursor" "keys1-up" (concatenate 'string *tab* *mode*))
       '(:tab-residue))
(check :phase-zero-bytes (reasons "keyboard-cursor" "keys1-up" (concatenate 'string *tab* *mode* *up*))
       '(:wrong-redraw))
(check :missing-tab (reasons "keyboard-cursor" "keys1-up" *up1*) '(:row-erase-count))

;;; Later phases from the pinned source (keyboard.c 1.48, 789-859 and 1016-1091).
;;; A VT52 phase sends CSI ?2l (keypad also ESC = or ESC >) before each read and
;;; ESC < after it (setup.c:419-428), so erase, echo and redraw stay ANSI CUP/SGR.
(defun hi (row col text) (format nil "^[~d;~dH^[7m~a^[m" row col text))
(defun header (labels mode prompt)
  "show_*(1) reverse LABELS (row col text), the <MODE> line and the row-22 prompt."
  (format nil "~{~a~}^[21;1H<~a>~20a^[22;1H^[0K^[22;1H~a"
          (loop for l in labels collect (apply #'hi l)) mode "" prompt))
(defparameter *cursor-labels* '((1 57 "UP") (1 62 "DN") (1 67 "LT") (1 72 "RT")))
(defparameter *keypad-labels*
  '((13 60 "PF1") (13 64 "PF2") (13 68 "PF3") (13 72 "PF4")
    (15 60 " 7 ") (15 64 " 8 ") (15 68 " 9 ") (15 72 " - ")
    (17 60 " 4 ") (17 64 " 5 ") (17 68 " 6 ") (17 72 " , ")
    (19 60 " 1 ") (19 64 " 2 ") (19 68 " 3 ")
    (21 60 "   0   ") (21 68 " . ") (21 72 "ENT")))
(defparameter *kp-prompt* "Press each function key. Finish with TAB.")
;; Each interval starts at the previous capture's target, after its loop-top bytes,
;; and ends at this capture's target, after the next loop-top bytes.
(defparameter *real-up1* ; keys1-up after phase 0's TAB, ANSI SET header.
  (x (concatenate 'string *tab*
                  "^[?1h" (header *cursor-labels* "ANSI / Cursor key mode SET" *prompt*)
                  "^[23;1H" *up1* "^[23;1H")))
(defparameter *vt52-up* ; keys2-up after phase 1's TAB; decckm(2) still sets ?1.
  (x (concatenate 'string *tab*
                  "^[?1h" (header *cursor-labels* "VT52 Mode" *prompt*)
                  "^[23;1H^[?2l^<"
                  "^[23;1H^[0K^[23;1H^[7m <27> A ^[m^[7m (Up arrow key) ^[m^[1;57HUP"
                  "^[23;1H^[?2l")))
(defparameter *vt52-kp1* ; keys2-kp_1 after phase 1's TAB.
  (x (concatenate 'string *tab*
                  (header *keypad-labels* "VT52 Numeric mode" *kp-prompt*)
                  "^[23;1H^[?2l^>^<" *kp1* "^[23;1H^[?2l^>")))
(defparameter *vt52-kp-enter* ; keys3-kp_enter after keys3-kp_2's target.
  (x (concatenate 'string "^<"
                  "^[23;1H^[0K^[23;1H^[7m <27> ? M ^[m^[7m (ENTER key) ^[m^[21;72HENT"
                  "^[23;1H^[?2l^=")))
(check :real-phase-header (reasons "keyboard-cursor" "keys1-up" *real-up1*) nil)
(check :vt52-cursor (reasons "keyboard-cursor" "keys2-up" *vt52-up*) nil)
(check :vt52-keypad (reasons "keyboard-keypad" "keys2-kp_1" *vt52-kp1*) nil)
(check :vt52-keypad-enter (reasons "keyboard-keypad" "keys3-kp_enter" *vt52-kp-enter*) nil)

;;; Final hold.
(check :pause (reasons "keyboard-cursor" "pause01" (concatenate 'string *tab* *hold*)) nil)
(check :pause-cut (reasons "keyboard-cursor" "pause01" (concatenate 'string *tab* *hold*) 0
                           (- (+ (length *tab*) (length *hold*)) 2))
       '(:no-hold-prompt))
(check :pause-no-tab (reasons "keyboard-cursor" "pause01" *hold*) '(:row-erase-count))
(check :pause-key-echo (reasons "keyboard-cursor" "pause01" (concatenate 'string *up1* *hold*))
       '(:no-tab-echo))

;;; Malformed or out-of-range intervals and identities.
(check :empty-interval (reasons "keyboard-cursor" "keys0-up" *up* 5 5) '(:malformed-interval))
(check :beyond-output (reasons "keyboard-cursor" "keys0-up" *up* 0 (1+ (length *up*))) '(:malformed-interval))
(check :negative-start (reasons "keyboard-cursor" "keys0-up" *up* -1) '(:malformed-interval))
(check :ratio-target (reasons "keyboard-cursor" "keys0-up" *up* 0 1/2) '(:malformed-interval))
(check :nil-target (reasons "keyboard-cursor" "keys0-up" *up* 0 nil) '(:malformed-interval))
(check :no-output (reasons "keyboard-cursor" "keys0-up" nil 0 1) '(:malformed-interval))
(check :unknown-id (reasons "keyboard-cursor" "keys9-up" *up*) '(:unknown-capture))
(check :unknown-label (reasons "keyboard-other" "keys0-up" *up*) '(:unknown-capture))

;;; Case aggregation binds each interval to the previous target.
(check :case-unknown (vttest-keyboard-row-output:case-row-output-errors "keyboard-other" *up* 0 nil)
       '(:unknown-case))
(check :case-short (vttest-keyboard-row-output:case-row-output-errors "keyboard-cursor" *up* 0 '(1 2))
       '(:target-count))
(check :case-dotted (vttest-keyboard-row-output:case-row-output-errors "keyboard-cursor" *up* 0 '(1 . 2))
       '(:target-count))
(let* ((o (concatenate 'string *prompt* *up* *down*))
       (a (+ (length *prompt*) (length *up*)))
       (errors (vttest-keyboard-row-output:case-row-output-errors
                "keyboard-cursor" o 0 (list* a (length o) (make-list 11 :initial-element (length o))))))
  (check :case-fresh-prefix (subseq errors 0 1)
         '(("keys0-left" :malformed-interval)))
  (check :case-error-count (length errors) 11))

(format t "~d checks, ~d failures~%" *checks* *failures*)
(unless (zerop *failures*) (sb-ext:exit :code 1))
