(in-package #:nerimux/terminal/actions)

(defconstant +tab-width+
  8
  "Standard terminal tab column interval: tab stops every 8 columns by default.")

(declaim (inline %cancel-wrap))

(defun %cancel-wrap (screen)
  "Cancel any pending (deferred) wrap.  Called by every explicit cursor movement:
   moving the cursor discards the VT100 last-column flag, so a subsequent write
   does not spuriously wrap.  See the screen-pending-wrap slot docstring."
  (setf (screen-pending-wrap screen) nil))

(defmacro define-cursor-movements (&rest specs)
  "Build cursor movement functions from a Prolog-like fact table.
   Each SPEC is (name docstring slot-accessor clamped-new-value-expr)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name docstring accessor limit-expr) spec
            `(defun ,name (screen n)
               ,docstring
               (%cancel-wrap screen)
               (setf (,accessor screen) ,limit-expr)
               (clamp-row-cursor screen))))
        specs)))

(defun set-cursor (screen x y)
  "Move the cursor to (X, Y), clamping both coordinates into bounds.
   Cancels a pending wrap (explicit positioning discards the last-column flag)."
  (%cancel-wrap screen)
  (setf (screen-cursor-y screen) (clamp y 0 (1- (screen-height screen)))
        (screen-cursor-x screen) (clamp x 0 (1- (row-columns screen))))
  (%clear-line-wrapped screen (screen-cursor-y screen)))

(define-cursor-movements
 (cursor-up "Move up N rows, stopping at the top margin unless already above it."
            screen-cursor-y
            (max (if (< (screen-cursor-y screen) (screen-scroll-top screen))
                     0 (screen-scroll-top screen))
                 (- (screen-cursor-y screen) n)))
 (cursor-down
  "Move down N rows, stopping at the bottom margin unless already below it."
  screen-cursor-y
  (min (if (> (screen-cursor-y screen) (screen-scroll-bottom screen))
           (1- (screen-height screen)) (screen-scroll-bottom screen))
       (+ (screen-cursor-y screen) n)))
 (cursor-right "Move the cursor right N columns, clamping to width-1."
               screen-cursor-x
               (min (1- (row-columns screen)) (+ (screen-cursor-x screen) n)))
 (cursor-left "Move the cursor left N columns, clamping to column 0."
              screen-cursor-x
              (max 0 (- (screen-cursor-x screen) n))))

(defun cursor-down/scroll (screen)
  "Move cursor down one line, scrolling when at the bottom of the scroll region.
   This is the internal helper used by write-char-at-cursor and the LF handler."
  (cond ((= (screen-cursor-y screen) (screen-scroll-bottom screen))
         (scroll-up-one screen))
        ((< (screen-cursor-y screen) (1- (screen-height screen)))
         (incf (screen-cursor-y screen))))
  (clamp-row-cursor screen))

(defun cursor-lf (screen)
  "Line feed: move cursor down, scrolling the scroll region if at the bottom."
  (%cancel-wrap screen)
  (cursor-down/scroll screen))

(defun cursor-nl (screen)
  "Process a C0 line-feed control (LF / VT / FF): a line feed, plus a carriage
   return when LNM (newline mode, CSI 20 h) is on so the cursor returns to column
   0.  IND (ESC D) calls cursor-lf directly and is therefore never affected by LNM."
  (cursor-lf screen)
  (when (screen-newline-mode screen)
    (setf (screen-cursor-x screen) 0)))

(defun %materialize-tab-stops (screen)
  "Return SCREEN's tab stops as a concrete sorted list of columns, expanding the
   :DEFAULT sentinel into the standard every-8-columns stops for the width."
  (let ((stops (screen-tab-stops screen)))
    (if (eq stops :default)
        (loop for c from 8 below (screen-width screen) by 8
              collect c)
        stops)))

(defun set-tab-stop (screen)
  "HTS (ESC H), set a horizontal tab stop at the current cursor column."
  (setf (screen-tab-stops screen) (sort
                                   (adjoin (screen-cursor-x screen)
                                           (%materialize-tab-stops screen))
                                   #'<)))

(defun clear-tab-stops (screen mode)
  "TBC: 0 clears the current horizontal stop, 3 clears all; other modes do nothing."
  (case mode
    (0 (setf (screen-tab-stops screen)
             (remove (screen-cursor-x screen) (%materialize-tab-stops screen))))
    (3 (setf (screen-tab-stops screen) nil))))

(defun %next-tab-stop (stops x max-x)
  "Return the column of the next tab stop after X.
   When STOPS is :DEFAULT the standard every-+TAB-WIDTH+-column grid is used;
   otherwise STOPS is a custom sorted list.  The result is clamped to MAX-X."
  (if (eq stops :default)
      (min (* +tab-width+ (ceiling (1+ x) +tab-width+)) max-x)
      (or
       (find-if
        (lambda (c)
          (> c x))
        (sort (copy-list stops) #'<))
       max-x)))

(defun %prev-tab-stop (stops x)
  "Return the column of the previous tab stop before X.
   When STOPS is :DEFAULT the standard every-+TAB-WIDTH+-column grid is used;
   otherwise STOPS is a custom sorted list.  The result is clamped to 0."
  (if (eq stops :default)
      (* +tab-width+ (floor (max 0 (1- x)) +tab-width+))
      (or
       (find-if
        (lambda (c)
          (< c x))
        (sort (copy-list stops) #'>))
       0)))

(defun cursor-ht (screen)
  "Horizontal tab: advance the cursor to the next tab stop (default: every
   +TAB-WIDTH+ columns; HTS/TBC can customise the stops), clamping to the last column."
  (%cancel-wrap screen)
  (setf (screen-cursor-x screen) (%next-tab-stop (screen-tab-stops screen)
                                                 (screen-cursor-x screen)
                                                 (1- (row-columns screen))))
  (clamp-row-cursor screen))

(defun cursor-cht (screen n)
  "CHT, cursor forward N tab stops (CSI N I).
   Advance the cursor to the Nth next tab stop, clamping to width-1."
  (dotimes (_ (max 1 n))
    (cursor-ht screen)))

(defun cursor-cbt (screen n)
  "CBT, cursor backward N tab stops (CSI N Z).
   Move the cursor back to the Nth previous tab stop, stopping at column 0."
  (%cancel-wrap screen)
  (dotimes (_ (max 1 n))
    (setf (screen-cursor-x screen) (%prev-tab-stop (screen-tab-stops screen)
                                                   (screen-cursor-x screen)))))

(defun cursor-bs (screen)
  "Backspace: move cursor left one column if not already at column 0."
  (%cancel-wrap screen)
  (when (> (screen-cursor-x screen) 0)
    (decf (screen-cursor-x screen))))

(defun cursor-ri (screen)
  "Reverse index (ESC M): move cursor up one line, scrolling down if at top."
  (%cancel-wrap screen)
  (cond ((= (screen-cursor-y screen) (screen-scroll-top screen))
         (scroll-down-one screen))
        ((plusp (screen-cursor-y screen))
         (decf (screen-cursor-y screen))))
  (clamp-row-cursor screen))

(defun cursor-cr (screen)
  "Carriage return: move the cursor to column 0."
  (%cancel-wrap screen)
  (setf (screen-cursor-x screen) 0)
  (%clear-line-wrapped screen (screen-cursor-y screen)))

(defun cursor-nel (screen)
  "NEL (ESC E), Next Line: carriage return then line feed, i.e. move the cursor
   to column 0 of the next row (scrolling at the bottom margin like LF)."
  (cursor-cr screen)
  (cursor-lf screen))
