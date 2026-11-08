;;;; Declarative vttest 20251205 main.c798..938 double-size references.
;;;; No terminal parser, product state, capture, or alternative-phase fitting.
(load (merge-pathnames "vttest-screen-oracle.lisp" *load-truename*))
(defpackage #:vttest-double-size-oracle
  (:use #:cl)
  (:export #:profile-error #:profile-error-field #:profile-error-value
           #:snapshot #:reference #:snapshot-pause #:snapshot-columns
           #:snapshot-cell #:snapshot-row-mode #:snapshot-metadata
           #:logical-columns #:scale-tile #:rasterize #:compare-reference
           #:+font-sha256+))
(in-package #:vttest-double-size-oracle)

(defconstant +font-sha256+
  "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(define-condition profile-error (error)
  ((field :initarg :field :reader profile-error-field)
   (value :initarg :value :reader profile-error-value))
  (:report (lambda (c s) (format s "Outside double-size reference profile: ~s = ~s"
                                (profile-error-field c) (profile-error-value c)))))
(defun checked (value predicate field)
  "Replacement values are revalidated; no restart silently admits a bad profile."
  (loop until (funcall predicate value) do
    (setf value (restart-case (error 'profile-error :field field :value value)
                  (use-value (replacement) :report "Supply a replacement profile value."
                    replacement))))
  value)
(defun boolean-p (x) (or (null x) (eq x t)))
(defun logical-columns (columns mode)
  (setf columns (checked columns (lambda (x) (member x '(80 132))) :columns)
        mode (checked mode (lambda (x) (member x '(:single :wide :top :bottom))) :row-mode))
  (if (eq mode :single) columns (/ columns 2)))

(defstruct (snapshot (:constructor %snapshot) (:conc-name %snapshot-))
  pause columns cells modes metadata)
(defun snapshot-pause (s) (%snapshot-pause s))
(defun snapshot-columns (s) (%snapshot-columns s))
(defun snapshot-row-mode (s row)
  (aref (%snapshot-modes s) (1- (checked row (lambda (x) (and (integerp x) (<= 1 x 24))) :row))))
(defun snapshot-cell (s row column)
  (let* ((mode (snapshot-row-mode s row))
         (c (checked column (lambda (x) (and (integerp x) (<= 1 x (logical-columns (snapshot-columns s) mode)))) :column)))
    (aref (%snapshot-cells s) (+ (* (1- row) (snapshot-columns s)) (1- c)))))
(defun snapshot-metadata (s) (copy-tree (%snapshot-metadata s)))
(defun blank-reference (pause columns)
  (%snapshot :pause pause :columns columns
             :cells (make-array (* 24 columns) :initial-element (vttest-screen-oracle:make-cell))
             :modes (make-array 24 :initial-element :single)))
(defun put-cell (s row column cell)
  (checked column (lambda (x) (and (integerp x) (<= 1 x (logical-columns (snapshot-columns s) (snapshot-row-mode s row))))) :column)
  (setf (aref (%snapshot-cells s) (+ (* (1- row) (snapshot-columns s)) (1- column))) cell))
(defun row-mode! (s row mode)
  "Explicit construction operation, not an escape-sequence interpreter.
  Width loss discards the right half; returning to single never restores it."
  (let ((limit (logical-columns (snapshot-columns s) mode)))
    (setf (aref (%snapshot-modes s) (1- row)) mode)
    (loop for c from (1+ limit) to (snapshot-columns s) do
      (setf (aref (%snapshot-cells s) (+ (* (1- row) (snapshot-columns s)) (1- c)))
            (vttest-screen-oracle:make-cell))))
  s)
(defun text! (s row column text &key bold blink)
  (loop for ch across text for c from column do
    (put-cell s row c (vttest-screen-oracle:make-cell :code (char-code ch) :bold bold :blink blink)))
  s)
(defun pattern-reference (pause)
  (let* ((large (> pause 2)) (offset (if large 13 0))
         (s (blank-reference pause (if large 132 80))))
    (text! s 1 1 (if large "132 column mode" " 80 column mode"))
    (text! s 5 (+ 3 (* 2 offset)) "v------- left margin")
    (text! s 7 (+ 3 (* 2 offset)) "This is a normal-sized line")
    (row-mode! s 9 :wide)
    (text! s 9 (+ 2 offset) "This is a Double-width line")
    (loop for row in '(11 12 14 15) for mode in '(:top :bottom :top :bottom) do
      (row-mode! s row mode)
      (text! s row (+ 2 offset) (if (< row 14) "This is a Double-width-and-height line"
                                  "This is another such line")))
    (text! s 17 (+ 3 (* 2 offset)) "^------- left margin")
    (text! s 21 1 (if (oddp pause) "This is not a double-width line" "This **is** a double-width line"))
    (when (evenp pause) (row-mode! s 21 :wide))
    (text! s 23 1 "Push <RETURN>")
    s))
(defun frame-reference ()
  (let ((s (blank-reference 5 80)))
    (loop for row from 8 to 17 do (row-mode! s row (if (evenp row) :top :bottom)))
    ;; DEC Table 3-9: l/q/k, x, m/q/j. Tabs installed at logical columns 9,17,...
    ;; Five HTs from column 2 terminate at the doubled-row right margin, column 40.
    (dolist (row '(8 9))
      (text! s row 1 (concatenate 'string "┌" (make-string 38 :initial-element #\─) "┐")))
    (loop for row from 10 to 15 do (text! s row 1 "│") (text! s row 40 "│"))
    (dolist (row '(16 17))
      (text! s row 1 (concatenate 'string "└" (make-string 38 :initial-element #\─) "┘")))
    (text! s 12 3 "* The mad programmer strikes again * " :bold t :blink t)
    ;; CUP(13,3), HT to 9, CUB6 to 3, with SGR1;5 still active.
    (text! s 13 3 "* The mad programmer strikes again *" :bold t :blink t)
    (text! s 22 1 "Another test pattern...  a frame with blinking bold text,")
    (text! s 23 1 "all in double-height double-width size. Push <RETURN>")
    s))
(defun half-box-reference ()
  (let ((old (frame-reference)) (s (blank-reference 6 80)))
    ;; RI twelve times at top of [8,24]: original8..12 -> final20..24.
    ;; New rows8..19 are single-width blank lines. Cells and line attributes move together.
    (loop for source from 8 to 12 for target from 20 do
      (row-mode! s target (snapshot-row-mode old source))
      (loop for c from 1 to 40 do (put-cell s target c (snapshot-cell old source c))))
    (text! s 1 1 "Exactly half of the box should remain. Push <RETURN>")
    s))
(defun reference (pause)
  "Six complete conditional visible references, derived solely from upstream.
  Entry: 24 rows, default colors, normal SGR/screen, ASCII G0 GL, full margins,
  absolute origin, autowrap on, normal insert mode, visible solid block cursor.
  Metadata records the content blink truth separately from an observed phase."
  (setf pause (checked pause (lambda (x) (and (integerp x) (<= 1 x 6))) :pause))
  (let* ((s (case pause (5 (frame-reference)) (6 (half-box-reference)) (t (pattern-reference pause))))
         (cursor-row (if (= pause 6) 1 23))
         (cursor-column (case pause ((1 2 3 4) 14) (5 54) (6 53))))
    (setf (%snapshot-metadata s)
          (list :cursor (list :row cursor-row :column cursor-column :visible t :style :block)
                :screen-reverse nil :content-blink (not (null (member pause '(5 6))))
                :required-blink-phase (if (= pause 5) t :observed-boolean)
                :current-pen '(:bold nil :underline nil :blink nil :inverse nil)
                :g0 :ascii :g1 (if (> pause 4) :ascii :inherited-unbound) :gl :g0
                :origin nil :margins '(1 24) :autowrap t :pending-wrap nil
                :row-modes (coerce (copy-seq (%snapshot-modes s)) 'list)
                :source-lines (case pause ((1 2) '(811 887)) ((3 4) '(811 887))
                                    (5 '(889 928)) (6 '(930 937)))
                :source-main-sha256 "4bd1d88d41c2cc346ae9ec858f11d87243eafbb829b4a0bf40168c45a364c3c1"
                :status :conditional))
    s))

(defun scale-tile (tile mode)
  "Compose an attributed 8x16 opaque tile into one physical row slice.
  DWL duplicates horizontally. DHL duplicates both axes then selects rows0..15
  or16..31. Attribute ordering is fixed in the single-cell primitive before scaling."
  (setf mode (checked mode (lambda (x) (member x '(:single :wide :top :bottom))) :row-mode)
        tile (checked tile (lambda (x) (and (vectorp x) (= 512 (length x))
                       (every (lambda (b) (typep b '(unsigned-byte 8))) x)
                       (loop for p from 3 below 512 by 4 always (= 255 (aref x p))))) :tile))
  (let* ((width (if (eq mode :single) 8 16))
         (image (make-array (* width 16 4) :element-type '(unsigned-byte 8))))
    (dotimes (y 16)
      (dotimes (x width)
        (let* ((sy (case mode (:top (floor y 2)) (:bottom (+ 8 (floor y 2))) (t y)))
               (sx (if (eq mode :single) x (floor x 2)))
               (from (* 4 (+ (* sy 8) sx))) (to (* 4 (+ (* y width) x))))
          (replace image tile :start1 to :start2 from :end2 (+ from 4)))))
    image))
(defun compose (s glyph-function phase)
  "Inert composition seam for hand-computed tile controls; no file access."
  (setf phase (checked phase #'boolean-p :blink-phase))
  (let* ((width (* 8 (snapshot-columns s)))
         (image (make-array (* width 384 4) :element-type '(unsigned-byte 8)))
         (cursor (getf (%snapshot-metadata s) :cursor)))
    (loop for row from 1 to 24 for mode = (snapshot-row-mode s row)
          for tile-width = (if (eq mode :single) 8 16) do
      (loop for c from 1 to (logical-columns (snapshot-columns s) mode)
            for cell = (snapshot-cell s row c)
            for tile = (scale-tile (vttest-screen-oracle:cell-rgba
                                    cell (funcall glyph-function (vttest-screen-oracle:cell-code cell))
                                    :blink-phase phase
                                    :cursor (and (getf cursor :visible) (= row (getf cursor :row))
                                                 (= c (getf cursor :column)))) mode) do
        (dotimes (y 16)
          (replace image tile :start1 (* 4 (+ (* (+ (* (1- row) 16) y) width) (* (1- c) tile-width)))
                              :start2 (* y tile-width 4) :end2 (* (1+ y) tile-width 4)))))
    image))
(defun nonzero-glyph (code index)
  (unless (and (integerp index) (plusp index))
    (error 'profile-error :field :glyph-index :value (list code index)))
  index)
(defun rasterize (s font &key (blink-phase nil phase-p) verified-font-sha256)
  "Caller must freshly verify SHA256 of these exact font bytes before calling.
  One stable observed READY phase only. Hold5 must be on to cover the bold text.
  No font fallback: a zero cmap glyph is a typed profile error."
  (setf blink-phase (checked (if phase-p blink-phase :missing) #'boolean-p :explicit-blink-phase)
        verified-font-sha256 (checked verified-font-sha256 (lambda (x) (equal x +font-sha256+)) :verified-font-sha256))
  (when (eq (getf (%snapshot-metadata s) :required-blink-phase) t)
    (setf blink-phase (checked blink-phase (lambda (x) (eq x t)) :coverage-blink-phase)))
  (vttest-cursor-oracle:font-info font)
  (let ((cache (make-hash-table)))
    (compose s (lambda (code)
                 (or (gethash code cache)
                     (setf (gethash code cache)
                           (progn
                             (nonzero-glyph code (vttest-cursor-oracle::cmap-glyph font code))
                             (vttest-cursor-oracle:glyph-rows font (code-char code))))))
             blink-phase)))
(defun compare-rgba (expected actual width)
  "All pixels and channels, zero tolerance; alpha is required opaque."
  (setf actual (checked actual (lambda (x) (and (vectorp x) (= (length x) (length expected))
                           (every (lambda (b) (typep b '(unsigned-byte 8))) x)
                           (loop for p from 3 below (length x) by 4 always (= 255 (aref x p))))) :rgba))
  (let ((different (loop for p from 0 below (length expected) by 4
                         count (loop for c below 4 thereis (/= (aref expected (+ p c)) (aref actual (+ p c)))))))
    (list :pass (zerop different) :compared-pixels (* width 384) :different-pixels different
          :width width :height 384 :tolerance 0 :mask nil)))
(defun compare-reference (s actual font &key (blink-phase nil phase-p) verified-font-sha256)
  (unless phase-p (error 'profile-error :field :explicit-blink-phase :value :missing))
  (compare-rgba (rasterize s font :blink-phase blink-phase :verified-font-sha256 verified-font-sha256)
                actual (* 8 (snapshot-columns s))))
