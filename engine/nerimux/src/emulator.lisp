(in-package #:nerimux/terminal/emulator)

(defun screen-process-bytes (screen bytes &key (start 0) (end (length bytes)) defer-geometry)
  "Feed BYTES[START..END). With DEFER-GEOMETRY, stop after the byte requesting
   a grid without resizing. Return the next byte index and whether we stopped."
  (let ((nerimux/terminal/actions::*defer-column-resize* defer-geometry)
        (generation (screen-geometry-generation screen)))
    (loop for i from start below end
          do (setf (screen-parser screen) (funcall (screen-parser screen) screen (aref bytes i)))
          when (and defer-geometry (/= generation (screen-geometry-generation screen)))
            do (return-from screen-process-bytes (values (1+ i) t))))
  (values end nil))
