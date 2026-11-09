;;;; Native prerequisite fixtures for vttest-keyboard-native-prereq-tests.lisp. NP-CASE
;;;; builds one fresh synthetic context per keyboard case whose events are shaped for
;;;; the real OWN-DA-REPLY-P, CASE-REPLY-END and CASE-TARGETS; NP-RUN calls the real
;;;; CASE-PROOF with counted loader, font, row and timing stubs and always restores
;;;; them. Reads no files. The synthetic output is not shaped for the real row proof,
;;;; so the row verdict is stubbed unless :ROW :REAL. Load after
;;;; vttest-keyboard-driver-tests.lisp and vttest-keyboard-native-prereq.lisp.
(in-package :vttest-keyboard-driver-tests)

(dolist (f '(k::case-proof k::load-case-context k::capture-expectations
             vttest-keyboard-row-output:case-row-output-errors))
  (unless (fboundp f) (error "Native prerequisite fixtures need ~s loaded first" f)))

(defparameter *np-labels* '("keyboard-cursor" "keyboard-keypad"))
(defparameter *np-dir* "/fixture/np/")
(defparameter *np-octets* (coerce '(0 24 36 66 126 66 66 0) '(vector (unsigned-byte 8)))
  "Stand-in octets for a passing font closure.")

(defun np-case (label &key (query k::+da-query+) (reply k::+da-reply+) (prefix "menu")
                            widths omit-barriers)
  "Fresh context for LABEL under *NP-DIR*, and the plist (:QUERY-END :REPLY-END
:TARGETS :NAMES). Events: QUERY written, REPLY enqueued and wired, PREFIX written;
then per CAPTURE-EXPECTATIONS entry its :KEY-BEGIN, WIDTH octets written (the Ith
of WIDTHS, default 4; 0 writes nothing, an empty interval), :KEY-END and a :CAPTURE
carrying its barrier. :REPLY-END is the first :KEY-BEGIN :OUT. BARRIERS is a
separately consed EQUAL alist in capture order without the OMIT-BARRIERS indices."
  (let ((events '()) (output "") (in 0) (barriers '()) (targets '()) (names '()) reply-end)
    (labels ((e (kind &rest args)
               (push (list* :kind kind :out (length output) :in in args) events))
             (write-out (bytes)
               (when (plusp (length bytes))
                 (e :wire-out :bytes bytes :end (+ (length output) (length bytes)))
                 (setf output (concatenate 'string output bytes)))))
      (write-out query)
      (e :enqueue :source :reply :bytes reply)
      (e :wire-in :source :reply :bytes reply :end (+ in (length reply)))
      (incf in (length reply))
      (write-out prefix)
      (setf reply-end (length output))
      (loop for (id key key-phase capture-phase) in (k::capture-expectations label)
            for i from 0
            for name = (format nil "~2,'0d-~a-~a" (1+ i) label id)
            do (e :key-begin :phase key-phase :key key)
               (write-out (make-string (or (nth i widths) 4) :initial-element #\x))
               (e :key-end :phase key-phase :key key)
               (let ((barrier (list :recorder-version 3 :name name :status :ready
                                    :target-output (length output))))
                 (e :capture :phase capture-phase :name name :barrier barrier)
                 (unless (member i omit-barriers)
                   (push (cons name (copy-tree barrier)) barriers)))
               (push name names)
               (push (length output) targets)))
    (values (k::make-case-context :label label :dir *np-dir* :events (reverse events)
                                  :output output :barriers (reverse barriers))
            (list :query-end (length query) :reply-end reply-end
                  :targets (reverse targets) :names (reverse names)))))

(defun np-captures (ctx)
  "CTX's :CAPTURE events in order; SETF GETF on an existing key edits CTX in place."
  (remove :capture (k::case-context-events ctx) :key (lambda (e) (getf e :kind))
                                                 :test-not #'eq))

(defun np-intervals (info)
  "(START . END) per capture from NP-CASE's INFO: the first starts at :REPLY-END,
each later one at the previous target."
  (loop for start = (getf info :reply-end) then end
        for end in (getf info :targets)
        collect (cons start end)))

(defun np-run (ctx &key (label (k::case-context-label ctx)) (dir (k::case-context-dir ctx))
                        (bytes :pass) (loaded ctx) (load-state :pass) load-reasons
                        (font (list :pass nil *np-octets*)) (row '()) (timing '(:pass nil)))
  "Call the real CASE-PROOF once with K::*PROOF-TRACE* collecting, then restore the
loader and row functions. The loader returns LOADED LOAD-STATE LOAD-REASONS. FONT
is (STATE REASONS OCTETS), a function such as a MAKE-FONT-PROOF closure, :SIGNAL,
or :ABSENT for no closure; the counted closure computes only when called. ROW is
the error list the row stub returns, or :REAL. TIMING is
(STATE REASONS) or :SIGNAL. Returns a plist: :RESULT the five CASE-PROOF values,
:TRACE (STEP . CTX) oldest first, :FONTS the closure call count, and argument
lists oldest first: :LOADS (LABEL DIR), :ROWS (LABEL OUTPUT REPLY-END TARGETS),
:TIMINGS (LABEL DIR EVENTS OUTPUT SCALE BARRIERS)."
  (let ((loads '()) (fonts 0) (rows '()) (timings '())
        (old-load (symbol-function 'k::load-case-context))
        (old-row (symbol-function 'vttest-keyboard-row-output:case-row-output-errors)))
    (flet ((font-closure ()
             (lambda ()
               (incf fonts)
              (cond ((eq font :signal) (error "np font signal"))
                    ((functionp font) (funcall font))
                    (t (values-list font)))))
           (timing-stub (label dir events output scale &key barriers)
             (push (list label dir events output scale barriers) timings)
             (if (eq timing :signal) (error "np timing signal") (values-list timing))))
      (unwind-protect
           (progn
             (setf (symbol-function 'k::load-case-context)
                   (lambda (l d) (push (list l d) loads) (values loaded load-state load-reasons)))
             (unless (eq row :real)
               (setf (symbol-function 'vttest-keyboard-row-output:case-row-output-errors)
                     (lambda (l o r ts) (push (list l o r ts) rows) row)))
             (let* ((k::*proof-trace* nil)
                    (result (multiple-value-list
                             (k::case-proof label dir bytes
                                            (if (eq font :absent) nil (font-closure))
                                            :timing #'timing-stub))))
               (list :result result :trace (reverse k::*proof-trace*)
                     :loads (reverse loads) :fonts fonts
                     :rows (reverse rows) :timings (reverse timings))))
        (setf (symbol-function 'k::load-case-context) old-load
              (symbol-function 'vttest-keyboard-row-output:case-row-output-errors) old-row)))))
