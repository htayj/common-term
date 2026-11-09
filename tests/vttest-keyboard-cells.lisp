;;;; Expected cells for vttest 20251205 keyboard tests 5.4 and 5.5 (keyboard.c 1.48).
;;;; Replays upstream draw order under the VT100 tables, which hold only when the
;;;; run's own bytes show DA reply ESC[?1;2c. Pure: no font, product or native input.
(load (merge-pathnames "vttest-screen-oracle.lisp" *load-truename*))

(defpackage #:vttest-keyboard-cells
  (:use #:cl)
  (:local-nicknames (#:o #:vttest-screen-oracle))
  (:export #:capture-ids #:expected-state #:tab-residue-state #:state-cell
           #:rasterize-state #:different-cells #:echo #:key-bytes))
(in-package #:vttest-keyboard-cells)

(defun spaces (n) (make-string n :initial-element #\Space))

(defun keytab ()
  "VT100_keytab (keyboard.c:69-124) as (r c symbol); always drawn reverse."
  (append
   (loop for (r c0 pairs) in '((1 6 "1!2@3#4$5%6^7&8*9(0)-_=+`~")
                               (2 8 "qQwWeErRtTyYuUiIoOpP[{]}")
                               (3 10 "aAsSdDfFgGhHjJkKlL;:'\"")
                               (4 12 "zZxXcCvVbBnNmM,<.>/?"))
         append (loop for i below (length pairs)
                      collect (list r (+ c0 (* 5 (floor i 2)) (mod i 2))
                                    (string (char pairs i)))))
   `((1 0 "ESC") (1 70 "BS") (2 0 " TAB ") (2 71 "DEL") (3 65 "RETN")
     (3 71 "\\") (3 72 "|") (4 69 "LF")
     (5 13 ,(concatenate 'string (spaces 16) "SPACE BAR" (spaces 16))))))

(defparameter +cursor-keys+
  '((0 56 "UP" "Up arrow") (0 61 "DN" "Down arrow")
    (0 66 "LT" "Left arrow") (0 71 "RT" "Right arrow"))
  "VT100_curkeytab, keyboard.c:199-202.")

(defparameter +keypad+
  '((6 59 "PF1" "PF1") (6 63 "PF2" "PF2") (6 67 "PF3" "PF3") (6 71 "PF4" "PF4")
    (7 59 " 7 " "Numeric 7") (7 63 " 8 " "Numeric 8") (7 67 " 9 " "Numeric 9")
    (7 71 " - " "Minus") (8 59 " 4 " "Numeric 4") (8 63 " 5 " "Numeric 5")
    (8 67 " 6 " "Numeric 6") (8 71 " , " "Comma") (9 59 " 1 " "Numeric 1")
    (9 63 " 2 " "Numeric 2") (9 67 " 3 " "Numeric 3") (10 59 "   0   " "Numeric 0")
    (10 67 " . " "Point") (10 71 "ENT" "ENTER"))
  "num_keypadtab, keyboard.c:267-284.")

(defparameter +cases+
  `(("keyboard-cursor" ,+cursor-keys+ ,+keypad+ (0 1 2 3) ("up" "down" "left" "right")
     ("ANSI / Cursor key mode RESET" "ANSI / Cursor key mode SET" "VT52 Mode")
     "Press each cursor key. Finish with TAB.")
    ("keyboard-keypad" ,+keypad+ ,+cursor-keys+ (12 13 17) ("kp_1" "kp_2" "kp_enter")
     ("ANSI Numeric mode" "ANSI Application mode" "VT52 Numeric mode"
      "VT52 Application mode")
     "Press each function key. Finish with TAB."))
  "Label, tested keys, other keys, pressed indices, capture names, mode labels, prompt.")

(defun case-of (label)
  (or (cdr (assoc label +cases+ :test #'equal)) (error "Unknown keyboard case ~s" label)))

(defun capture-ids (label)
  "Phase-ordered key captures, then the final hold."
  (destructuring-bind (keys other pressed names labels prompt) (case-of label)
    (declare (ignore keys other pressed prompt))
    (append (loop for p below (length labels)
                  append (loop for n in names collect (format nil "keys~d-~a" p n)))
            '("pause01"))))

(defun key-bytes (label phase k)
  "Bytes for the K-th pressed key, as tests/vttest-protocol.lisp expected-keys."
  (let ((esc (string (code-char 27))))
    (if (string= label "keyboard-cursor")
        (concatenate 'string esc (nth phase '("[" "O" "")) (string (char "ABDC" k)))
        (if (evenp phase)
            (string (char (format nil "12~c" #\Return) k))
            (concatenate 'string esc (if (= phase 1) "O" "?") (string (char "qrM" k)))))))

(defun echo (bytes)
  "chrprint2: a leading space, then chrformat tokens (main.c:1802-1806)."
  (format nil " ~{~a~}"
          (loop for ch across bytes for c = (char-code ch)
                collect (if (or (<= c 32) (>= c 127)) (format nil "<~d> " c) (format nil "~c " ch)))))

(defun replay (label phase count &key tab hold)
  "Cells after COUNT keys of PHASE; earlier phases are complete through their TAB.
TAB also processes this phase's TAB; HOLD adds the final erase and holdit text."
  (destructuring-bind (keys other pressed names labels prompt) (case-of label)
    (declare (ignore names))
    (let ((cells (make-array 1920 :initial-element (o:make-cell))))
      (labels ((put (row column string inverse)
                 (loop for ch across string for col from column
                       do (setf (aref cells (+ (* (1- row) 80) (1- col)))
                                (o:make-cell :code (char-code ch) :inverse inverse))))
               (erase (row) (put row 1 (spaces 80) nil))
               (key (entry inverse)
                 (put (+ 1 (* 2 (first entry))) (+ 1 (second entry)) (third entry) inverse))
               (show-tab () (erase 23) (put 23 1 (echo (string #\Tab)) t)))
        (dolist (entry (keytab)) (key entry t))
        (dolist (entry other) (key entry nil))
        (loop for p from 0 to phase do
          (dolist (entry keys) (key entry t))
          (put 21 1 (format nil "<~a>~20a" (nth p labels) "") nil)
          (erase 22)
          (put 22 1 prompt nil)
          (dotimes (k (if (= p phase) count (length pressed)))
            (let ((text (echo (key-bytes label p k))) (entry (nth (nth k pressed) keys)))
              (erase 23)
              (put 23 1 text t)
              (put 23 (1+ (length text)) (format nil " (~a key) " (fourth entry)) t)
              (key entry nil)))
          (when (or (< p phase) tab) (show-tab)))
        (when hold (erase 23) (put 23 1 "Push <RETURN>" nil)))
      cells)))

(defun state (cells row column)
  (list :cells cells :screen-reverse nil :cursor (list :row row :column column :visible t)))

(defun expected-state (label id)
  "Expected cells and metadata for capture ID of LABEL."
  (destructuring-bind (keys other pressed names labels prompt) (case-of label)
    (declare (ignore keys other prompt))
    (cond ((string= id "pause01")
           (state (replay label (1- (length labels)) (length pressed) :tab t :hold t) 23 14))
          ((not (member id (capture-ids label) :test #'string=))
           (error "Unknown ~a capture ~s" label id))
          (t (let ((phase (digit-char-p (char id 4)))
                   (k (position (subseq id 6) names :test #'string=)))
               (state (replay label phase (1+ k)) 23 1))))))

(defun tab-residue-state (label phase)
  "PHASE begun, no key processed yet: the previous TAB echo may persist (rejection control)."
  (state (replay label phase 0) 23 1))

(defun state-cell (state row column)
  (aref (getf state :cells) (+ (* (1- row) 80) (1- column))))

(defun rasterize-state (state glyph-rows &key (blink-phase nil phase-p))
  "640x384 RGBA through cell-rgba. GLYPH-ROWS maps a code to sixteen bytes.
The cursor depends only on visibility and position; BLINK-PHASE is passed through."
  (assert phase-p () "blink-phase must be explicit")
  (let ((image (make-array (* 640 384 4) :element-type '(unsigned-byte 8)))
        (cursor (getf state :cursor)))
    (dotimes (r 24 image)
      (dotimes (c 80)
        (let* ((cell (state-cell state (1+ r) (1+ c)))
               (tile (o:cell-rgba cell (funcall glyph-rows (o:cell-code cell))
                                  :blink-phase blink-phase
                                  :screen-reverse (getf state :screen-reverse)
                                  :cursor (and (getf cursor :visible)
                                               (= (1+ r) (getf cursor :row))
                                               (= (1+ c) (getf cursor :column))))))
          (dotimes (y 16)
            (replace image tile :start1 (* 4 (+ (* (+ (* r 16) y) 640) (* c 8)))
                                :start2 (* y 32) :end2 (* (1+ y) 32))))))))

(defun different-cells (a b)
  "One-based (row column) of every 8x16 cell whose RGBA differs between A and B."
  (loop for r below 24 nconc
    (loop for c below 80
          unless (loop for y below 16 for p = (* 4 (+ (* (+ (* r 16) y) 640) (* c 8)))
                       always (equalp (subseq a p (+ p 32)) (subseq b p (+ p 32))))
            collect (list (1+ r) (1+ c)))))
