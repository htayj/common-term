(in-package #:nerimux/terminal/types)

(defun %copy-overlapping-cells (screen old-cells old-width copy-cols copy-rows)
  "Copy the top-left COPY-COLS x COPY-ROWS rectangle from OLD-CELLS (a raw
   vector with OLD-WIDTH stride) into SCREEN's freshly installed grid."
  (dotimes (y copy-rows)
    (dotimes (x copy-cols)
      (setf (screen-cell screen x y) (aref old-cells (+ (* y old-width) x))))))

(defun screen-resize (screen new-width new-height)
  "Resize SCREEN to NEW-WIDTH x NEW-HEIGHT in place, preserving the
   overlapping top-left rectangle of content.  Resets the scroll region to
   the full new height and clamps the cursor into bounds.

   Saved alternate cells use the same stride and are resized with the active grid.

   Callers that share the screen with a reader thread must hold SCREEN's
   lock; this function does no locking of its own."
  (when (and (= new-width  (screen-width  screen))
             (= new-height (screen-height screen)))
    (return-from screen-resize screen))
  (let* ((old-width  (screen-width  screen))
         (old-height (screen-height screen))
         (old-cells  (screen-cells  screen)))
    (when (screen-alt-cells screen)
      (let ((saved (%make-blank-cells (* new-width new-height))))
        (dotimes (y (min old-height new-height))
          (dotimes (x (min old-width new-width))
            (setf (aref saved (+ (* y new-width) x))
                  (aref (screen-alt-cells screen) (+ (* y old-width) x)))))
        (setf (screen-alt-cells screen) saved
              (screen-alt-cursor-x screen) (min (screen-alt-cursor-x screen) (1- new-width))
              (screen-alt-cursor-y screen) (min (screen-alt-cursor-y screen) (1- new-height)))))
    (setf (screen-cells  screen) (%make-blank-cells (* new-width new-height))
          (screen-width  screen) new-width
          (screen-height screen) new-height)
    (%copy-overlapping-cells screen old-cells old-width
                             (min old-width new-width) (min old-height new-height))
    ;; Retain only visible row designations, and discard characters beyond a
    ;; doubled row's new logical margin so growing again cannot resurrect them.
    (maphash (lambda (row size)
               (declare (ignore size))
               (if (>= row new-height)
                   (remhash row (screen-line-sizes screen))
                   (loop for col from (max 1 (floor new-width 2)) below new-width
                         do (setf (screen-cell screen col row) (blank-cell)))))
             (screen-line-sizes screen))
    (setf (screen-scroll-top    screen) 0
          (screen-scroll-bottom screen) (1- new-height)
          (screen-cursor-y      screen) (clamp (screen-cursor-y screen) 0 (1- new-height))
          (screen-cursor-x screen)
          (clamp (screen-cursor-x screen) 0
                 (1- (if (gethash (screen-cursor-y screen) (screen-line-sizes screen))
                         (max 1 (floor new-width 2)) new-width)))
          (screen-pending-wrap screen) nil
          (screen-dirty-p       screen) t)
    (%clear-all-line-wrapped screen)
    screen))
