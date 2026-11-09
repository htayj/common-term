;;;; Offline controls for tests/vttest-keyboard-cells.lisp with synthetic glyph rows.
;;;; Pure model checks; they are not native evidence.
(load (merge-pathnames "vttest-keyboard-cells.lisp" *load-truename*))

(defpackage #:vttest-keyboard-cells-tests
  (:use #:cl)
  (:local-nicknames (#:k #:vttest-keyboard-cells) (#:o #:vttest-screen-oracle)))
(in-package #:vttest-keyboard-cells-tests)

(defvar *pass* 0)
(defvar *fail* 0)

(defun check (name ok)
  (if ok (incf *pass*) (progn (incf *fail*) (format t "FAIL ~s~%" name)))
  ok)

(defun glyph (code)
  "Synthetic sixteen bytes: blank for space, a nonzero code-dependent pattern otherwise."
  (let ((rows (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (unless (= code 32)
      (dotimes (y 16) (setf (aref rows y) (logior 1 (ldb (byte 8 0) (+ code (* 37 y)))))))
    rows))

(defun raster (state phase) (k:rasterize-state state #'glyph :blink-phase phase))

(defun text (state row column width)
  (map 'string (lambda (c) (code-char (o:cell-code (k:state-cell state row c))))
       (loop for c from column below (+ column width) collect c)))

(defun inverse-p (state row column width)
  "T when every cell is reverse, NIL when every cell is plain, :MIXED otherwise."
  (let ((flags (loop for c from column below (+ column width)
                     collect (o:cell-inverse (k:state-cell state row c)))))
    (cond ((every #'identity flags) t) ((notany #'identity flags) nil) (t :mixed))))

(defun all-p (state entries inverse)
  (every (lambda (e) (eq inverse (inverse-p state (first e) (second e) (third e)))) entries))

(defun tile-solid-p (image row column rgb)
  (loop for y below 16 always
    (loop for x below 8
          for p = (* 4 (+ (* (+ (* (1- row) 16) y) 640) (* (1- column) 8) x))
          always (and (= (aref image p) (first rgb)) (= (aref image (+ p 1)) (second rgb))
                      (= (aref image (+ p 2)) (third rgb)) (= (aref image (+ p 3)) 255)))))

(defun moved (state row column)
  (list :cells (getf state :cells) :screen-reverse (getf state :screen-reverse)
        :cursor (list :row row :column column :visible t)))

(defun cells-equal-p (a b)
  (every (lambda (x y)
           (every (lambda (f) (eql (funcall f x) (funcall f y)))
                  (list #'o:cell-code #'o:cell-inverse #'o:cell-bold
                        #'o:cell-underline #'o:cell-blink)))
         (getf a :cells) (getf b :cells)))

;;; Label, capture names, mode labels, pressed (row column width) in press order,
;;; tested keys never pressed, other key group, first echo text.
(defparameter +cases+
  '(("keyboard-cursor" ("up" "down" "left" "right")
     ("ANSI / Cursor key mode RESET" "ANSI / Cursor key mode SET" "VT52 Mode")
     ((1 57 2) (1 62 2) (1 67 2) (1 72 2)) ()
     ((13 60 3) (21 60 7) (21 72 3)) " <27> [ A ")
    ("keyboard-keypad" ("kp_1" "kp_2" "kp_enter")
     ("ANSI Numeric mode" "ANSI Application mode" "VT52 Numeric mode"
      "VT52 Application mode")
     ((19 60 3) (19 64 3) (21 72 3)) ((13 60 3) (15 60 3))
     ((1 57 2) (1 62 2) (1 67 2) (1 72 2)) " 1 ")))

(defun run-case (label names labels pressed unpressed other echo)
  (let ((ids (k:capture-ids label)) (last nil))
    (check (list label :ids)
           (equal ids (append (loop for p below (length labels)
                                    append (loop for n in names
                                                 collect (format nil "keys~d-~a" p n)))
                              '("pause01"))))
    (check (list label :count) (= 13 (length ids)))
    (dolist (id ids)
      (let* ((s (k:expected-state label id))
             (final (string= id "pause01"))
             (column (if final 14 1))
             (phase (if final (1- (length labels)) (digit-char-p (char id 4))))
             (done (if final (length names)
                       (1+ (position (subseq id 6) names :test #'string=)))))
        (setf last s)
        (check (list id :meta)
               (and (= 1920 (length (getf s :cells))) (null (getf s :screen-reverse))
                    (equal (getf s :cursor) (list :row 23 :column column :visible t))))
        (check (list id :no-blink) (notany #'o:cell-blink (getf s :cells)))
        (check (list id :cursor-cell)
               (let ((c (k:state-cell s 23 column)))
                 (and (= 32 (o:cell-code c)) (eq (o:cell-inverse c) (not final)))))
        (check (list id :main) (and (string= "ESC" (text s 3 1 3)) (eq t (inverse-p s 3 1 3))))
        (check (list id :other-plain) (all-p s other nil))
        (check (list id :unpressed-reverse) (all-p s unpressed t))
        (check (list id :label)
               (let ((l (format nil "<~a>" (nth phase labels))))
                 (string= l (text s 21 1 (length l)))))
        (check (list id :pressed)
               (loop for e in pressed for i from 0
                     always (eq (inverse-p s (first e) (second e) (third e)) (>= i done))))
        (let ((a (raster s t)) (b (raster s nil)))
          (check (list id :phase-t-nil) (null (k:different-cells a b)))
          (check (list id :cursor-rgb)
                 (tile-solid-p a 23 column (if final '(235 219 178) '(29 32 33))))
          (when (equal id (first ids))
            (check (list id :echo) (and (string= echo (text s 23 1 (length echo)))
                                        (eq t (inverse-p s 23 1 (length echo)))))
            (check (list id :move)
                   (equal (k:different-cells a (raster (moved s 5 10) nil))
                          '((5 10) (23 1))))))))
    (check (list label :hold)
           (and (string= "Push <RETURN>" (text last 23 1 13)) (null (inverse-p last 23 1 80))))
    (loop for p from 1 below (length labels) do
      (let ((r (k:tab-residue-state label p))
            (first-key (k:expected-state label (format nil "keys~d-~a" p (first names)))))
        (check (list label p :residue)
               (and (string= " <9> " (text r 23 1 5)) (eq t (inverse-p r 23 1 5))
                    (all-p r pressed t) (all-p r other nil)))
        (check (list label p :raster-rejected)
               (find 23 (k:different-cells (raster r nil) (raster first-key nil)) :key #'first))
        (loop for q from p below (length labels) do
          (check (list label p q :rejected)
                 (not (cells-equal-p r (k:expected-state
                                        label (format nil "keys~d-~a" q (first names)))))))))
    (length ids)))

(handler-case
    (let ((total (loop for c in +cases+ sum (apply #'run-case c))))
      (check :total-26 (= 26 total))
      (format t "~&keyboard-cells controls: ~d passed, ~d failed, ~d states~%"
              *pass* *fail* total)
      (finish-output)
      (sb-ext:exit :code (if (zerop *fail*) 0 1)))
  (error (e)
    (format *error-output* "~&ERROR ~a~%" e)
    (finish-output *error-output*)
    (sb-ext:exit :code 2)))
