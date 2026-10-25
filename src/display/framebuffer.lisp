;;;; Lispterm framebuffer. Native Lisp display implementation.

(in-package #:lispterm.display)

(defun resize-display (d width height)
  (setf (display-width d) (max 1 width)
        (display-height d) (max 1 height)
        (display-columns d) (max 1 (floor width (font-width (display-font d))))
        (display-rows d) (max 1 (floor height (font-height (display-font d))))
        (display-framebuffer d)
          (make-array (* (display-width d) (display-height d)) :element-type
                      '(unsigned-byte 32) :initial-element 4280098849)
        (display-grid d)
          (make-array (list (display-rows d) (display-columns d)) :initial-element nil)
        (display-damage d) (list (list 0 0 (display-width d) (display-height d))))
  (make-resize-event :columns (display-columns d) :rows (display-rows d) :width width :height
   height))

(defun render-cell (d row col cell)
  (let* ((f (display-font d))
         (cw (font-width f))
         (ch (font-height f))
         (x (* col cw))
         (y (* row ch))
         (span (max 1 (cell-width cell)))
         (fg (or (cell-fg cell) 15457202))
         (bg (or (cell-bg cell) 1908769))
         (cursor
          (and (display-cursor-visible d) (eql row (display-cursor-row d))
               (eql col (display-cursor-col d)))))
    (when (not (eql (not (null (cell-inverse cell))) (not (null cursor)))) (rotatef fg bg))
    (let ((glyph (glyph-bitmap f (cell-char cell)))
          (fb (display-framebuffer d))
          (stride (display-width d)))
      (dotimes (dy ch)
        (dotimes (dx (* span cw))
          (when (and (< (+ x dx) stride) (< (+ y dy) (display-height d)))
            (setf (aref fb (+ x dx (* (+ y dy) stride)))
                    (logior 4278190080
                            (if (or (and (< dx cw) (= 1 (aref glyph (+ dx (* dy cw)))))
                                    (and (cell-bold cell) (> dx 0) (<= dx cw)
                                         (= 1 (aref glyph (+ (1- dx) (* dy cw)))))
                                    (and (cell-underline cell) (= dy (- ch 2))))
                                fg
                                bg)))))))
    (push (list x y (min (* span cw) (- (display-width d) x)) ch) (display-damage d))))

(defun same-cell-p (a b)
  "Compare all CELL fields for framebuffer cache reuse, with case-sensitive characters.
Keep this comparison in sync with the CELL structure when adding render state."
  (and a b
       (eql (cell-char a) (cell-char b))
       (eql (cell-width a) (cell-width b))
       (eql (cell-fg a) (cell-fg b))
       (eql (cell-bg a) (cell-bg b))
       (eql (cell-bold a) (cell-bold b))
       (eql (cell-inverse a) (cell-inverse b))
       (eql (cell-underline a) (cell-underline b))))

(defun draw-cells (d row column cells)
  (assert-owner d)
  (loop for cell across (coerce cells 'vector)
        for col from column
        when (and (<= 0 row) (< row (display-rows d)) (<= 0 col) (< col (display-columns d)))
        do (unless (same-cell-p cell (aref (display-grid d) row col))
             (setf (aref (display-grid d) row col) (copy-cell cell))
             (unless (zerop (cell-width cell)) (render-cell d row col cell))))
  d)

(defun set-cursor (d row col &key (visible t))
  (assert-owner d)
  (let ((old-row (display-cursor-row d)) (old-col (display-cursor-col d)))
    (setf (display-cursor-visible d) nil)
    (when
        (and old-row old-col (<= 0 old-row) (<= 0 old-col) (< old-row (display-rows d))
             (< old-col (display-columns d)))
      (let ((cell (aref (display-grid d) old-row old-col)))
        (when cell (render-cell d old-row old-col cell))))
    (setf (display-cursor-row d) row
          (display-cursor-col d) col
          (display-cursor-visible d) visible)
    (when
        (and visible row col (<= 0 row) (< row (display-rows d)) (<= 0 col)
             (< col (display-columns d)))
      (let ((cell (aref (display-grid d) row col)))
        (when cell (render-cell d row col cell)))))
  d)

(defun draw-frame (d grid &key cursor-row cursor-col cursor-visible)
  (dotimes (row (min (array-dimension grid 0) (display-rows d)))
    (draw-cells d row 0
     (loop for col below (min (array-dimension grid 1) (display-columns d))
           collect (aref grid row col))))
  (set-cursor d cursor-row cursor-col :visible cursor-visible)
  d)
