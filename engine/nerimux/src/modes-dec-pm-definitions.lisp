(in-package #:nerimux/terminal/actions)

(defmacro define-dec-pm-rules (&rest specs)
  "Generate DEC-PM-SET and DEC-PM-RESET from a single Prolog-like rule table.
   Each SPEC is (param (set-body...) (reset-body...)).
   Unknown mode numbers are accepted silently."
  `(progn
     (defun dec-pm-set (screen params)
       "Handle DEC private mode set sequences (?XXXh)."
       (declare (ignorable screen))
       (dolist (param params)
         (case param
           ,@(mapcar
              (lambda (s)
                `(,(car s) ,@(cadr s)))
              specs))))
     (defun dec-pm-reset (screen params)
       "Handle DEC private mode reset sequences (?XXXl)."
       (declare (ignorable screen))
       (dolist (param params)
         (case param
           ,@(mapcar
              (lambda (s)
                `(,(car s) ,@(caddr s)))
              specs))))))

(defun set-column-mode (screen wide-p)
  "DECCOLM executes synchronously, even when the requested width is unchanged."
  (let ((columns (if wide-p 132 80)))
    (screen-resize screen columns (screen-height screen))
    (setf (screen-cells screen) (%make-blank-cells (* columns (screen-height screen)))
          (screen-column-mode screen) wide-p
          (screen-geometry-columns screen) columns
          (screen-scroll-top screen) 0
          (screen-scroll-bottom screen) (1- (screen-height screen))
          (screen-cursor-x screen) 0 (screen-cursor-y screen) 0
          (screen-pending-wrap screen) nil (screen-dirty-p screen) t)
    (clrhash (screen-line-sizes screen))
    (nerimux/terminal/types::%clear-all-line-wrapped screen)
    (incf (screen-geometry-generation screen))))

(define-dec-pm-rules
  (3 ((set-column-mode screen t)) ((set-column-mode screen nil)))
  (2
   ((values)) ; DECANM can only be set with ESC < while in VT52 mode.
   ((setf (screen-vt52-mode screen) t
          (screen-vt52-graphics screen) nil
          (screen-pending-wrap screen) nil)
    (return-from dec-pm-reset nil))) ; Following ANSI parameters do not cross modes.

  (25
   ((setf (screen-cursor-visible screen) t))
   ((setf (screen-cursor-visible screen) nil)))

  (6
   ((setf (screen-origin-mode screen) t)
    (set-cursor screen 0 (screen-scroll-top screen)))
   ((setf (screen-origin-mode screen) nil)
    (set-cursor screen 0 0)))

  (5
   ((setf (screen-reverse-screen screen) t
          (screen-dirty-p screen) t))
   ((setf (screen-reverse-screen screen) nil
          (screen-dirty-p screen) t)))

  (1
   ((setf (screen-app-cursor-keys screen) t))
   ((setf (screen-app-cursor-keys screen) nil)))

  (2004
   ((setf (screen-bracketed-paste screen) t))
   ((setf (screen-bracketed-paste screen) nil)))

  (7
   ((setf (screen-autowrap screen) t))
   ((setf (screen-autowrap screen) nil)))

  (1004
   ((setf (screen-focus-events screen) t))
   ((setf (screen-focus-events screen) nil)))

  (1049
   ((enter-alt-screen screen :save-cursor-p t))
   ((exit-alt-screen screen :restore-cursor-p t)))

  (2026
   ((values))
   ((values)))

  (47
   ((enter-alt-screen screen))
   ((exit-alt-screen screen)))

  (2048
   ((values))
   ((values)))

  (1047
   ((enter-alt-screen screen))
   ((exit-alt-screen screen)))

  (1048
   ((save-cursor screen))
   ((restore-cursor screen)))

  (12
   ((values))
   ((values))))
