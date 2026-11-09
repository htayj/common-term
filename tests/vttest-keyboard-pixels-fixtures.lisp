;;;; Pixel gating fixtures: one case context per label with literal ordered
;;;; captures, a matching EQUAL barrier alist and fixed font octets, plus stubs that
;;;; count capture, glyph, tool and comparison work. Reads no files, runs no tools.
;;;; Load after vttest-keyboard-driver-tests.lisp and the keyboard driver, -context,
;;;; -cells, -decoder, -png and -pixels files.
(in-package :vttest-keyboard-driver-tests)

(dolist (f '(k::case-pixels k::capture-pixel-status k::pixel-aggregate k::make-case-context
             k::capture-pixels k::capture-ids vttest-keyboard-cells:capture-ids
             vttest-keyboard-cells:expected-state vttest-keyboard-decoder:oracle-rows))
  (unless (fboundp f) (error "Pixel fixtures need ~s loaded first" f)))

(defparameter *pixel-labels* '("keyboard-cursor" "keyboard-keypad"))

(defun pixel-ids (label)
  "Literal capture ids: cursor phases 0-2 of four arrows, keypad phases 0-3 of
three keys, each followed by pause01."
  (multiple-value-bind (phases keys)
      (if (equal label "keyboard-cursor")
          (values 3 '("up" "down" "left" "right"))
          (values 4 '("kp_1" "kp_2" "kp_enter")))
    (append (loop for p below phases
                  append (loop for key in keys collect (format nil "keys~d-~a" p key)))
            (list "pause01"))))

(defun pixel-name (label id)
  "Capture basename NN-LABEL-ID, NN the one-based position of ID."
  (format nil "~2,'0d-~a-~a" (1+ (position id (pixel-ids label) :test #'equal)) label id))

(defun pixel-names (label)
  (mapcar (lambda (id) (pixel-name label id)) (pixel-ids label)))

(defparameter *pixel-octets*
  (coerce '(0 0 24 36 66 66 126 66 66 66 66 0 0 0 0 0) '(vector (unsigned-byte 8)))
  "Fixed stand-in font octets. The glyph stub accepts only these, by EQ, and
returns them as the sixteen rows of every code.")

(defun pixel-barrier (name out)
  "Barrier plist for capture NAME targeting transcript offset OUT."
  (list :recorder-version 3 :name name :status :ready :target-output out))

(defun pixel-context (label &key (names (pixel-names label)) (octets *pixel-octets*))
  "Fresh context for LABEL: a key event, then one :CAPTURE per name at :OUT 10, 20 ...
carrying its barrier, and a separately consed EQUAL barrier alist in that order."
  (let ((events (list (list :kind :key-begin :key "Up" :out 0))) (barriers '()))
    (loop for name in names for out from 10 by 10
          do (push (list :kind :capture :name name :out out :barrier (pixel-barrier name out))
                   events)
             (push (cons name (pixel-barrier name out)) barriers))
    (k::make-case-context :label label :dir "/fixture/run/" :events (reverse events)
                          :output (make-string (* 10 (1+ (length names))) :initial-element #\a)
                          :barriers (reverse barriers) :oracle-octets octets)))

(defun pixel-event (ctx name)
  (find name (k::case-context-events ctx) :key (lambda (e) (getf e :name)) :test #'equal))

(defun pixel-barrier-entry (ctx name)
  (assoc name (k::case-context-barriers ctx) :test #'equal))

(defvar *capture-calls* '() "(DIR NAME EXPECTED SCRATCH) per stubbed capture, newest first.")
(defvar *glyph-calls* 0 "Stubbed ORACLE-ROWS calls.")

(defun pixel-pass (name)
  "Default stubbed capture result: pass with a hash and raw path naming NAME."
  (values :pass nil (concatenate 'string "sha-" name) (concatenate 'string "raw-" name)))

(defun call-with-pixel-stubs (thunk &key (result #'pixel-pass) extra)
  "Call THUNK with CAPTURE-PIXELS answering (FUNCALL RESULT NAME), ORACLE-ROWS
returning *PIXEL-OCTETS*, the magick hook refusing, and EXTRA ((symbol . function))
installed afterwards. Every replaced definition is restored on any exit."
  (let* ((stubs (append
                 (list (cons 'k::capture-pixels
                             (lambda (dir name expected scratch)
                               (push (list dir name expected scratch) *capture-calls*)
                               (funcall result name)))
                       (cons 'vttest-keyboard-decoder:oracle-rows
                             (lambda (bytes code)
                               (declare (ignore code))
                               (incf *glyph-calls*)
                               (unless (eq bytes *pixel-octets*) (error "Foreign font octets"))
                               *pixel-octets*)))
                 extra))
         (old (mapcar (lambda (s) (cons (car s) (fdefinition (car s)))) stubs)))
    (unwind-protect
         (let ((*capture-calls* '()) (*glyph-calls* 0)
               (k::*pixel-tool* (lambda (args) (error "Pixel tool reached: ~s" args))))
           (dolist (s stubs) (setf (fdefinition (car s)) (cdr s)))
           (funcall thunk))
      (dolist (s (reverse old)) (setf (fdefinition (car s)) (cdr s))))))

(defun run-pixels (label ctx prereq timing &rest options)
  "One CASE-PIXELS call under the stubs with *PROOF-TRACE* collecting. A plist of
:STATUS :ENTRIES :REASONS, :TRACE, :CALLS oldest first, :GLYPHS, and the :TOOLS
and :COMPARISONS started. An escaped error becomes :STATUS :ERROR."
  (let ((tools k::*pixel-tool-calls*) (comparisons k::*pixel-comparisons*)
        (k::*proof-trace* nil))
    (apply #'call-with-pixel-stubs
           (lambda ()
             (handler-case
                 (multiple-value-bind (status entries reasons)
                     (k::case-pixels label ctx prereq timing :scratch "/fixture/scratch/")
                   (list :status status :entries entries :reasons reasons
                         :trace k::*proof-trace* :calls (reverse *capture-calls*)
                         :glyphs *glyph-calls*
                         :tools (- k::*pixel-tool-calls* tools)
                         :comparisons (- k::*pixel-comparisons* comparisons)))
               (error (e) (list :status :error :reasons (list (princ-to-string e))))))
           options)))

(defun untouched-p (r)
  "No capture, glyph, magick or comparison work happened in run R."
  (and (null (getf r :calls)) (eql 0 (getf r :glyphs))
       (eql 0 (getf r :tools)) (eql 0 (getf r :comparisons))))

(defun refused-p (r status reason)
  "R is STATUS without entries, REASON first, and nothing was decoded."
  (and (eq status (getf r :status)) (null (getf r :entries))
       (equal reason (first (getf r :reasons))) (untouched-p r)))

(defun traced-once-p (r ctx)
  "The run's proof trace is exactly one :PIXELS step on CTX itself (EQ)."
  (let ((trace (getf r :trace)))
    (and (= 1 (length trace)) (eq :pixels (car (first trace))) (eq ctx (cdr (first trace))))))

;; Shared by the gating and runtime suites: ENTRIES are passing entries of both cases.
(defun aggregate-controls (entries)
  "ENTRIES: passing entries of both cases. Only exact coverage passes; FAIL dominates."
  (flet ((agg (checks &optional n) (k::pixel-aggregate checks n))
         (with (i status) (let ((c (mapcar #'copy-list entries))) (setf (getf (nth i c) :pixels) status) c)))
    (let ((n (length entries)))
      (check (and (= 26 n) (eq :pass (agg entries)) (eq :pass (agg entries n))) :aggregate-pass)
      (check (every (lambda (c) (eq :unverified (agg (first c) (second c))))
                    (list (list entries (1- n)) (list (rest entries)) (list nil)
                          (list (remove "keyboard-keypad" entries :key (lambda (e) (getf e :label)) :test #'equal))
                          (list (append (butlast entries) (list (first entries))))
                          (list (cons (list :label "keyboard-cursor" :id "keys9-up" :pixels :pass) (rest entries)))
                          (list (with 3 :unverified))))
             :aggregate-unverified)
      (check (and (eq :fail (agg (with 3 :fail))) (eq :fail (agg (cons (first entries) (with 0 :fail)) 99)))
             :aggregate-fail-dominates))))
