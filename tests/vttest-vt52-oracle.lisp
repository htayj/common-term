;;;; Independent expected screen for vttest 20251205 VT52 test, pause 2 of 3.
;;;; Source: .build/evidence/cursor-x11-first-screen-20261106-a/vttest-20251205.tgz
;;;; (sha256 cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc),
;;;; member vttest-20251205/vt52.c v1.26 (sha256 720867fb...712d4) lines 159-177:
;;;;   vt52home; vt52ed; printxx("This is the normal character set:");
;;;;   rows 3+j, column 16: bytes 32+i+48j, i=0..47, j=0..1;
;;;;   vt52cup(6,1) caption; ESC F; same bytes at rows 8+j, column 16; ESC G;
;;;;   vt52cup(12,1); holdit() = tprintf("Push <RETURN>") (unix_io.c:237-241).
;;;; esc.c:1430 vt52cup(l,c) sends ESC Y (l+31) (c+31). A VT52 decodes ESC Y Pl Pc
;;;; as zero-based line Pl-32 and column Pc-32, so l and c are ONE-BASED: zero-based
;;;; (l-1, c-1). vt52cup(1,1) is home and vt52cup(max_lines,10) is the last row.
;;;; COORDINATES: every row/column in this API is one-based. Byte #x5F is at
;;;; one-based row 9 column 31 = zero-based row 8 column 30.
;;;; Byte 127 (DEL, i=47 of j=1) is ignored and occupies no cell.
;;;; Graphics bytes use the approved DEC special graphics map (map-byte #\0),
;;;; never the renderer mapper or a captured grid. VT52 has no SGR: all cells normal.
(load (merge-pathnames "vttest-charsets-oracle.lisp" *load-truename*))

(defpackage #:vttest-vt52-oracle
  (:use #:cl)
  (:export #:vt52-snapshot #:snapshot-pause #:snapshot-rows #:snapshot-columns
           #:snapshot-cells #:snapshot-cell #:snapshot-metadata
           #:+graphics-row+ #:+graphics-first-column+ #:graphics-cells
           #:boundary-cells #:source-text-rows))
(in-package #:vttest-vt52-oracle)

(defconstant +graphics-row+ 9
  "Row of special-graphics bytes 80..126 (vt52cup(8+1,16)).")
(defconstant +graphics-first-column+ 31
  "Column of byte #x5F: 16 + (#x5F - 80). Byte #x7E lands at column 62.")

(defstruct (snapshot (:constructor %snapshot (cells)) (:copier nil)
                     (:conc-name %snapshot-))
  (cells nil :read-only t))

(defun snapshot-pause (snapshot) (check-type snapshot snapshot) 2)
(defun snapshot-rows (snapshot) (check-type snapshot snapshot) 24)
(defun snapshot-columns (snapshot) (check-type snapshot snapshot) 80)
(defun snapshot-cells (snapshot)
  "Fresh row-major vector of 1920 immutable vttest-screen-oracle cells."
  (check-type snapshot snapshot)
  (copy-seq (%snapshot-cells snapshot)))
(defun snapshot-cell (snapshot row column)
  "Immutable cell at one-based ROW, COLUMN."
  (check-type snapshot snapshot)
  (check-type row (integer 1 24))
  (check-type column (integer 1 80))
  (aref (%snapshot-cells snapshot) (+ (* (1- row) 80) (1- column))))

(defun snapshot-metadata (snapshot)
  "Fresh reference contract for the second VT52 hold, not capture admission."
  (check-type snapshot snapshot)
  (list :status :complete-reference :area (copy-seq "vt52-mode")
        :label (copy-seq "vt52") :path (list 7) :pause 2
        :coordinates :one-based :rows 24 :columns 80 :width 640 :height 384
        ;; holdit's 13-character prompt occupies row 12 columns 1..13.
        :cursor (list :row 12 :column 14 :visible t)
        :screen-reverse nil :mode :vt52 :graphics-mode nil
        :current-pen (list :bold nil :underline nil :blink nil :inverse nil
                           :foreground :default :background :default)
        :source-archive-sha256
        (copy-seq "cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc")
        :source-member (copy-seq "vttest-20251205/vt52.c")
        :source-vt52-sha256
        (copy-seq "720867fb58f57ba70cdfc12643c94d256278e325fdf43d691a440fc16ab712d4")
        :source-vt52-lines (list 159 177)
        :ignored-del 2 :graphics-map :dec-special-graphics))

(defun source-text-rows ()
  "Fresh (row column string) for the literal source strings."
  (list (list 1 1 (copy-seq "This is the normal character set:"))
        (list 6 1 (copy-seq "This is the special graphics character set:"))
        (list 12 1 (copy-seq "Push <RETURN>"))))

(defun vt52-snapshot ()
  "All 1920 cells of the second VT52 hold, built from vt52.c alone."
  (let ((cells (make-array 1920 :initial-element (vttest-screen-oracle:make-cell))))
    (flet ((put (row column code)
             (setf (aref cells (+ (* (1- row) 80) (1- column)))
                   (vttest-screen-oracle:make-cell :code code))))
      (loop for (row column string) in (source-text-rows)
            do (loop for ch across string for col from column
                     do (put row col (char-code ch))))
      (loop for (first-row designator) in '((3 #\B) (8 #\0))
            do (dotimes (j 2)
                 (dotimes (i 48)
                   (let ((byte (+ 32 i (* 48 j))))
                     (unless (= byte 127)
                       (put (+ first-row j) (+ 16 i)
                            (vttest-charsets-oracle:map-byte designator byte))))))))
    (%snapshot cells)))

(defun graphics-cells (snapshot)
  "Fresh list of 32 (:byte B :row R :column C :code P), bytes #x5F..#x7E."
  (loop for byte from #x5f to #x7e
        for column from +graphics-first-column+
        collect (list :byte byte :row +graphics-row+ :column column
                      :code (vttest-screen-oracle:cell-code
                             (snapshot-cell snapshot +graphics-row+ column)))))

(defun boundary-cells ()
  "Fresh (row column) list of cells surrounding the 32 graphics cells:
row 9 columns 30 and 63, rows 8 and 10 over columns 30..63, and the
normal-set row 4 columns 31..62 holding the same bytes as ASCII."
  (let ((left (1- +graphics-first-column+))
        (right (+ +graphics-first-column+ 32)))
    (append (list (list +graphics-row+ left) (list +graphics-row+ right))
            (loop for row in (list (1- +graphics-row+) (1+ +graphics-row+))
                  nconc (loop for column from left to right
                              collect (list row column)))
            (loop for column from +graphics-first-column+ below right
                  collect (list 4 column)))))
