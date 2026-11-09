;;;; Pure layout tests for tests/vttest-vt52-oracle.lisp. No font, decoder or capture.
;;;; Run: .build/deps/bin/sbcl --no-userinit --no-sysinit --non-interactive \
;;;;        --load tests/vttest-vt52-oracle-tests.lisp
(load (merge-pathnames "vttest-vt52-oracle.lisp" *load-truename*))

(defpackage #:vttest-vt52-oracle-tests (:use #:cl #:vttest-vt52-oracle))
(in-package #:vttest-vt52-oracle-tests)

(defvar *failures* 0)
(defvar *checks* 0)
(defmacro check (name form)
  `(progn (incf *checks*)
          (unless (ignore-errors ,form)
            (incf *failures*)
            (format t "FAIL ~a~%" ,name))))

;; Approved VT100 Table 3-9 literal, engine/nerimux/t/vt52-tests.lisp:120.
(defparameter *literal32* " ◆▒␉␌␍␊°±␤␋┘┐┌└┼⎺⎻─⎼⎽├┤┴┬│≤≥π≠£·")

(defun cup-zero-based (l c)
  "Zero-based line/column a VT52 decodes from vt52cup(l,c): ESC Y (l+31)-32 (c+31)-32."
  (list (- (+ l 31) 32) (- (+ c 31) 32)))

(defun ascii (from to) (map 'string #'code-char (loop for b from from to to collect b)))

(defun expected-row (row)
  "Independent 80-character row text from vt52.c lines 159-177."
  (let ((line (make-string 80 :initial-element #\Space)))
    (flet ((at (column text) (replace line text :start1 (1- column))))
      (case row
        (1 (at 1 "This is the normal character set:"))
        (3 (at 16 (ascii 32 79)))
        (4 (at 16 (ascii 80 126)))
        (6 (at 1 "This is the special graphics character set:"))
        (8 (at 16 (ascii 32 79)))
        (9 (at 16 (concatenate 'string (ascii 80 94) *literal32*)))
        (12 (at 1 "Push <RETURN>"))))
    line))

(defun row-text (snapshot row)
  (let ((line (make-string 80)))
    (dotimes (c 80 line)
      (setf (char line c)
            (code-char (vttest-screen-oracle:cell-code (snapshot-cell snapshot row (1+ c))))))))

(defun plain-cell-p (cell)
  (and (= 1 (vttest-screen-oracle:cell-width cell))
       (eq :single (vttest-screen-oracle:cell-line-size cell))
       (eq :default (vttest-screen-oracle:cell-foreground cell))
       (eq :default (vttest-screen-oracle:cell-background cell))
       (notany (lambda (f) (funcall f cell))
               (list #'vttest-screen-oracle:cell-bold #'vttest-screen-oracle:cell-underline
                     #'vttest-screen-oracle:cell-blink #'vttest-screen-oracle:cell-inverse))))

(let* ((s (vt52-snapshot))
       (cells (snapshot-cells s))
       (graphics (graphics-cells s))
       (meta (snapshot-metadata s)))
  (check :literal-length (= 32 (length *literal32*)))
  (check :geometry (and (= 24 (snapshot-rows s)) (= 80 (snapshot-columns s))
                        (= 2 (snapshot-pause s)) (= 1920 (length cells))))
  (check :all-cells-plain (every #'plain-cell-p cells))
  (dotimes (r 24)
    (check (list :row (1+ r)) (string= (expected-row (1+ r)) (row-text s (1+ r)))))
  (check :special-row-8-identity
         (loop for b from 32 to 79 always
               (= b (vttest-screen-oracle:cell-code (snapshot-cell s 8 (- b 16))))))
  (check :del-ignored (and (= 126 (vttest-screen-oracle:cell-code (snapshot-cell s 4 62)))
                           (= 32 (vttest-screen-oracle:cell-code (snapshot-cell s 4 63)))))
  (check :graphics-count (= 32 (length graphics)))
  (loop for g in graphics for i from 0 for literal across *literal32*
        do (check (list :graphics i)
                  (and (= (getf g :byte) (+ #x5f i))
                       (= (getf g :row) 9 +graphics-row+)
                       (= (getf g :column) (+ 31 i) (+ 16 (- (getf g :byte) 80)))
                       (= (getf g :code) (char-code literal)
                          (vttest-charsets-oracle:map-byte #\0 (getf g :byte))))))
  ;; Coordinate basis: API is one-based; derive the 0x5F cell from the escape encoding.
  (check :coordinate-basis
         (and (eq :one-based (getf meta :coordinates))
              (destructuring-bind (r0 c0) (cup-zero-based 9 16)
                (and (= 8 r0) (= 15 c0)
                     (= 31 (+ 1 c0 (- #x5f 80)) +graphics-first-column+)
                     (= 9 (1+ r0) +graphics-row+)))))
  (check :blank-5f (= 32 (getf (first graphics) :code)))
  (check :left-boundary
         (= #x5e (vttest-screen-oracle:cell-code (snapshot-cell s 9 30))))
  (check :right-boundary
         (= 32 (vttest-screen-oracle:cell-code (snapshot-cell s 9 63))))
  (let ((boundary (boundary-cells)))
    (check :boundary-count (= (+ 2 34 34 32) (length boundary)))
    (check :boundary-disjoint
           (notany (lambda (rc) (and (= 9 (first rc)) (<= 31 (second rc) 62))) boundary))
    (check :boundary-ascii
           (every (lambda (rc) (< (vttest-screen-oracle:cell-code
                                   (apply #'snapshot-cell s rc)) 127))
                  boundary))
    (check :ascii-control-row
           (equal (loop for rc in boundary when (= 4 (first rc))
                        collect (vttest-screen-oracle:cell-code (apply #'snapshot-cell s rc)))
                  (loop for b from #x5f to #x7e collect b))))
  (check :cursor (equal '(:row 12 :column 14 :visible t) (getf meta :cursor)))
  (check :cursor-after-prompt
         (and (= (char-code #\>) (vttest-screen-oracle:cell-code (snapshot-cell s 12 13)))
              (= 32 (vttest-screen-oracle:cell-code (snapshot-cell s 12 14)))))
  (check :metadata (and (eql 2 (getf meta :pause)) (eql 2 (getf meta :ignored-del))
                        (equal '(7) (getf meta :path))
                        (= 64 (length (getf meta :source-vt52-sha256)))))
  (check :fresh-cells (progn (setf (aref cells 0) nil)
                             (= 84 (vttest-screen-oracle:cell-code (snapshot-cell s 1 1)))))
  (check :row-range (handler-case (progn (snapshot-cell s 25 1) nil) (type-error () t)))
  (check :column-range (handler-case (progn (snapshot-cell s 1 0) nil) (type-error () t))))

(format t "vt52-oracle: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
