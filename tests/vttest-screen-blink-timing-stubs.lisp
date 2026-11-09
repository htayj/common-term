;;;; Stub closure for blink renderer tests: only src/package, src/conditions,
;;;; src/session and src/renderer load, over a one-cell fake engine and display.
(defpackage #:lispterm.display (:use #:cl) (:nicknames #:ltd)
  (:export #:display-geometry-request #:geometry-current-p #:draw-frame #:present
           #:set-cursor #:set-title #:make-cell #:focus-event #:focus-event-focused-p
           #:key-event #:key-event-bytes #:paste-event #:paste-event-text #:resize-event
           #:resize-event-columns #:resize-event-rows #:text-event #:text-event-text))
(defpackage #:lispterm.pty (:use #:cl)
  (:export #:process-poll #:pty-process #:pty-read #:pty-resize #:pty-window-size #:pty-write))
(defpackage #:nerimux/terminal/types (:use #:cl)
  (:export #:screen-focus-events #:screen-geometry-generation))
(defpackage #:nerimux/terminal (:use #:cl)
  (:export #:screen-app-cursor-keys #:screen-app-keypad #:screen-bracketed-paste
           #:screen-clipboard-queue #:screen-notification-queue #:screen-passthrough-queue
           #:screen-response-queue #:screen-vt52-mode #:screen-cursor-visible #:screen-title))
(defpackage #:babel (:use #:cl) (:export #:string-to-octets))
(let ((here *load-truename*))
  (dolist (file '("../src/package" "../src/conditions" "../src/session" "../src/renderer"
                  "vttest-screen-blink-timing"))
    (load (merge-pathnames (concatenate 'string file ".lisp") here))))
(defpackage #:vttest-screen-blink-timing-tests (:use #:cl #:vttest-screen-blink-timing))
(in-package #:vttest-screen-blink-timing-tests)
;;; A one-cell stub screen: CHAR, BLINK, CURSOR and DIRTY are all it has.
(defstruct fake (char #\X) (blink t) (cursor 0) (dirty t))
(defvar *fail-draw* nil)
(macrolet ((stubs (&rest specs)
             `(progn ,@(loop for (name args . body) in specs
                             collect `(defun ,name ,args (declare (ignorable ,@(set-difference args lambda-list-keywords))) ,@body)))))
  (stubs (lispterm.engine:rows (s) 1) (lispterm.engine:columns (s) 1)
         (lispterm.engine:logical-columns (s r) 1) (lispterm.engine:line-size (s r) :normal)
         (lispterm.engine:cell-at (s r c) s) (lispterm.engine:cell-character (s) (fake-char s))
         (lispterm.engine:cell-width (s) 1) (lispterm.engine:cell-foreground (s) 7)
         (lispterm.engine:cell-background (s) 0) (lispterm.engine:cell-bold-p (s) nil)
         (lispterm.engine:cell-inverse-p (s) nil) (lispterm.engine:cell-underline-p (s) nil)
         (lispterm.engine:cell-blink-p (s) (fake-blink s))
         (lispterm.engine:cursor-row (s) 0) (lispterm.engine:cursor-column (s) (fake-cursor s))
         (lispterm.engine:reverse-screen-p (s) nil) (lispterm.engine:dirty-p (s) (fake-dirty s))
         (lispterm.engine:clear-damage (s) (setf (fake-dirty s) nil))
         (nerimux/terminal:screen-cursor-visible (s) t) (nerimux/terminal:screen-title (s) nil)
         (ltd:display-geometry-request (d) nil) (ltd:set-title (d title) nil)
         (ltd:make-cell (&rest r) r) (ltd:present (d) nil) (ltd:set-cursor (d r c &key visible) nil)
         (ltd:draw-frame (d grid &key cursor-row cursor-col cursor-visible reverse-screen blink-phase)
           (when *fail-draw* (error "Injected draw failure")))))
