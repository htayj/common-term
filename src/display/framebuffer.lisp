;;;; Backend-neutral cell rasterizer and damage cache.
(in-package #:lispterm.display)

(defun resize-display (d width height)
  (assert-owner d)
  (setf (display-width d) (max 1 width)
        (display-height d) (max 1 height)
        (display-columns d) (max 1 (floor width (font-width (display-font d))))
        (display-rows d) (max 1 (floor height (font-height (display-font d))))
        (display-framebuffer-storage d)
        (when (and (eq (display-renderer-kind d) :cpu)
                   (or (eq (display-renderer-request d) :cpu)
                       (display-renderer-fallback-reason d)))
          (make-array (* (display-width d) (display-height d))
                      :element-type '(unsigned-byte 32) :initial-element #xff1d2021))
        (display-grid d)
        (make-array (list (display-rows d) (display-columns d)) :initial-element nil)
        (display-damage d) (list (list 0 0 (display-width d) (display-height d))))
  (when (eq (display-renderer-kind d) :gpu)
    (gpu-call "RENDERER-RESIZE" (display-renderer-state d)
              (display-width d) (display-height d)
              (font-width (display-font d)) (font-height (display-font d)))
    (setf (display-renderer-pending-p d) t (display-readback-pending-p d) t))
  (make-resize-event :columns (display-columns d) :rows (display-rows d)
                     :width width :height height))

(defun cell-x-scale (cell)
  (if (eq (cell-line-size cell) :single) 1 2))

(defun cell-colors (d row col cell &key (cursor-p t))
  "Resolve inverse, screen reverse and cursor inversion exactly once."
  (let ((fg (or (cell-fg cell) #xebdbb2)) (bg (or (cell-bg cell) #x1d2021)))
    (when (oddp (count-if #'identity
                         (list (cell-inverse cell) (display-reverse-screen d)
                               (and cursor-p (display-cursor-visible d)
                                    (eql row (display-cursor-row d))
                                    (eql col (display-cursor-col d))))))
      (rotatef fg bg))
    (values fg bg)))

(defun render-gpu-cell (d row col cell)
  (assert-owner d)
  (multiple-value-bind (foreground background) (cell-colors d row col cell)
    (let* ((font (display-font d)) (cw (font-width font)) (ch (font-height font))
           (span (max 1 (cell-width cell))) (sx (cell-x-scale cell))
           (x (* col cw sx)) (y (* row ch))
           (right (min (display-width d)
                       (* cw sx (max 1 (floor (display-columns d) sx)))))
           ;; Only glyph coverage is built on the CPU, never frame composition.
           (glyph (glyph-bitmap font (cell-char cell)))
           (mask (make-array (length glyph) :element-type '(unsigned-byte 8))))
      (dotimes (i (length glyph))
        (setf (aref mask i) (if (zerop (aref glyph i)) 0 255)))
      (gpu-call "RENDERER-WRITE-CELL" (display-renderer-state d) col row
                :glyph-key (list (string (cell-char cell)) (not (null (cell-bold cell))) span)
                :mask mask :foreground foreground :background background
                :bold (cell-bold cell) :underline (cell-underline cell)
                :hidden (and (cell-blink cell) (not (display-blink-phase d)))
                :span span :line-size (cell-line-size cell))
      (setf (display-renderer-pending-p d) t (display-readback-pending-p d) t)
      (when (< x right)
        (push (list x y (min (* span cw sx) (- right x)) ch) (display-damage d))))))

(defun render-cell (d row col cell)
  "Keep a normal function dispatch so live redefinition affects cached drawing."
  (if (eq (display-renderer-kind d) :gpu)
      (render-gpu-cell d row col cell)
      (render-cpu-cell d row col cell)))

(defun render-cpu-cell (d row col cell)
  (let* ((f (display-font d)) (cw (font-width f)) (ch (font-height f))
         (sx (cell-x-scale cell)) (size (cell-line-size cell))
         (double-height (member size '(:double-height-top :double-height-bottom)))
         (x (* col cw sx)) (y (* row ch))
         (span (max 1 (cell-width cell)))
         (right (min (display-width d)
                     (* cw sx (max 1 (floor (display-columns d) sx)))))
         (fg (or (cell-fg cell) #xebdbb2)) (bg (or (cell-bg cell) #x1d2021))
         (cursor (and (display-cursor-visible d) (eql row (display-cursor-row d))
                      (eql col (display-cursor-col d)))))
    ;; Screen reverse, cell inverse and cursor inversion compose by parity.
    (when (oddp (count-if #'identity
                         (list (cell-inverse cell) (display-reverse-screen d) cursor)))
      (rotatef fg bg))
    (let ((glyph (glyph-bitmap f (cell-char cell)))
          (fb (display-framebuffer d)) (stride (display-width d)))
      (dotimes (dy ch)
        (let ((gy (if double-height
                      (floor (+ dy (if (eq size :double-height-bottom) ch 0)) 2)
                      dy)))
          (dotimes (dx (* span cw sx))
            (let ((gx (floor dx sx)))
              (when (and (< (+ x dx) right) (< (+ y dy) (display-height d)))
                (setf (aref fb (+ x dx (* (+ y dy) stride)))
                      (logior #xff000000
                              (if (and (or (not (cell-blink cell)) (display-blink-phase d))
                                       (or (and (< gx cw) (= 1 (aref glyph (+ gx (* gy cw)))))
                                           (and (cell-bold cell) (> gx 0) (<= gx cw)
                                                (= 1 (aref glyph (+ (1- gx) (* gy cw)))))
                                           (and (cell-underline cell) (= gy (- ch 2)))))
                                  fg bg)))))))))
    (when (< x right)
      (push (list x y (min (* span cw sx) (- right x)) ch)
            (display-damage d)))))

(defun same-cell-p (a b)
  "Compare EVERY visible CELL field, using case-sensitive character equality."
  (and a b
       (eql (cell-char a) (cell-char b)) (eql (cell-width a) (cell-width b))
       (eql (cell-fg a) (cell-fg b)) (eql (cell-bg a) (cell-bg b))
       (eql (cell-bold a) (cell-bold b)) (eql (cell-inverse a) (cell-inverse b))
       (eql (cell-underline a) (cell-underline b))
       (eql (cell-blink a) (cell-blink b)) (eql (cell-line-size a) (cell-line-size b))))

(defun draw-cells (d row column cells)
  (assert-owner d)
  (loop for cell across (coerce cells 'vector) for col from column
        when (and (<= 0 row) (< row (display-rows d)) (<= 0 col)
                  (< col (max 1 (floor (display-columns d) (cell-x-scale cell)))))
        do (unless (same-cell-p cell (aref (display-grid d) row col))
             ;; Publish cache success only after the selected renderer accepts the cell.
             (unless (zerop (cell-width cell)) (render-cell d row col cell))
             (setf (aref (display-grid d) row col) (copy-cell cell))))
  d)

(defun set-cursor (d row col &key (visible t))
  (assert-owner d)
  (let ((old-row (display-cursor-row d)) (old-col (display-cursor-col d)))
    (setf (display-cursor-visible d) nil)
    (when (and old-row old-col (<= 0 old-row) (<= 0 old-col)
               (< old-row (display-rows d)) (< old-col (display-columns d)))
      (let ((cell (aref (display-grid d) old-row old-col)))
        (when cell (render-cell d old-row old-col cell))))
    (setf (display-cursor-row d) row (display-cursor-col d) col
          (display-cursor-visible d) visible)
    (when (and visible row col (<= 0 row) (< row (display-rows d))
               (<= 0 col) (< col (display-columns d)))
      (let ((cell (aref (display-grid d) row col)))
        (when cell (render-cell d row col cell)))))
  d)

(defun paint-row-remainder (d row start cell)
  "Extend the last logical cell's background, excluding cursor inversion.
Cell inverse and screen reverse apply; blink affects no background pixels."
  (assert-owner d)
  (let* ((cw (font-width (display-font d))) (ch (font-height (display-font d)))
         (x (* start cw)) (y (* row ch)) (width (- (display-width d) x))
         (height (min ch (- (display-height d) y)))
         (color (logior #xff000000
                        (if (not (eql (not (null (cell-inverse cell)))
                                      (not (null (display-reverse-screen d)))))
                            (or (cell-fg cell) #xebdbb2)
                            (or (cell-bg cell) #x1d2021)))))
    (if (eq (display-renderer-kind d) :gpu)
        (progn
          (gpu-call "RENDERER-WRITE-REMAINDER" (display-renderer-state d)
                    x y width height (logand color #xffffff))
          (setf (display-renderer-pending-p d) t (display-readback-pending-p d) t))
        (dotimes (dy height)
          (fill (display-framebuffer d) color
                :start (+ x (* (+ y dy) (display-width d)))
                :end (* (1+ (+ y dy)) (display-width d)))))
    (push (list x y width height) (display-damage d))))

(defun draw-frame (d grid &key cursor-row cursor-col cursor-visible
                            reverse-screen (blink-phase t))
  "Draw logical rows through the common X11/Wayland framebuffer interface."
  (assert-owner d)
  (let ((global-change (or (not (eql reverse-screen (display-reverse-screen d)))
                           (not (eql blink-phase (display-blink-phase d))))))
    (setf (display-reverse-screen d) reverse-screen (display-blink-phase d) blink-phase)
    (dotimes (row (min (array-dimension grid 0) (display-rows d)))
      (let* ((first (aref grid row 0)) (previous (aref (display-grid d) row 0))
             (scale (cell-x-scale first))
             (columns (min (array-dimension grid 1)
                           (max 1 (floor (display-columns d) scale))))
             (last (aref grid row (1- columns)))
             (old-last (aref (display-grid d) row (1- columns)))
             (row-change (or global-change
                             (not (and previous (eq (cell-line-size first)
                                                     (cell-line-size previous)))))))
        (when row-change
          (dotimes (col (display-columns d))
            (setf (aref (display-grid d) row col) nil)))
        (draw-cells d row 0 (loop for col below columns collect (aref grid row col)))
        ;; Odd physical widths leave a strip outside the doubled logical cells.
        ;; A one-column display instead clips its single doubled cell in place.
        (when (and (= scale 2) (< (* columns scale) (display-columns d))
                   (or row-change (not (same-cell-p last old-last))))
          (paint-row-remainder d row (* columns scale) last)))))
  (set-cursor d cursor-row cursor-col :visible cursor-visible)
  d)
