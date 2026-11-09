;;;; Reply boundary and capture targets for the keyboard row-23 output proof.
;;;; Pure: inert lists and strings in, values out. No files, recorder, display
;;;; or adapter. Load after vttest-area.lisp, which loads vttest-protocol.lisp
;;;; (capture-name-owned-p) and vttest-keyboard-driver.lisp; the driver defines
;;;; this package, CASE-OF and CAPTURE-IDS, and loads vttest-keyboard-artifacts.lisp
;;;; (CAPTURE-ID) and vttest-keyboard-timing.lisp (CAPTURE-EXPECTATIONS).
;;;; Call order: OWN-DA-REPLY-P must already have returned T for the same EVENTS
;;;; and OUTPUT; then CASE-REPLY-END, CASE-TARGETS, and only when both succeed
;;;; vttest-keyboard-row-output:case-row-output-errors. :MISSING-BARRIER means
;;;; unverified; every other refusal is a failure.
(in-package #:vttest-keyboard-driver)

(defun prereq-plist-p (x)
  "X is a proper, noncircular list of even length."
  (let ((n (ignore-errors (list-length x))))
    (and n (evenp n))))

(defun prereq-events-p (events)
  "EVENTS is a proper, noncircular list of proper even plists."
  (and (ignore-errors (list-length events))
       (every #'prereq-plist-p events)))

(defun prereq-barriers-p (barriers)
  "BARRIERS is a proper alist of capture name strings to proper even plists."
  (and (ignore-errors (list-length barriers))
       (every (lambda (b) (and (consp b) (stringp (car b)) (prereq-plist-p (cdr b))))
              barriers)))

(defun prereq-kind (e) (getf e :kind))

(defun prereq-reply-p (e)
  (and (member (getf e :kind) '(:enqueue :wire-in))
       (eq (getf e :source) :reply)))

(defun case-reply-end (label events)
  "Return the :OUT of the first :KEY-BEGIN, where LABEL's first capture interval
starts, and NIL; otherwise NIL and :ARGUMENTS, :NO-KEY-BEGIN, :FIRST-KEY,
:BOUNDARY or :REPLY-AFTER-KEY.

That key must be LABEL's first expected key in phase keys-0. Offsets never
decrease, so no earlier event's :OUT exceeds the result, though equal offsets
occur. Output written before that key, a stale row-23 redraw included, lies
outside every interval. OWN-DA-REPLY-P proves the seven-octet reply and the event
progression; :REPLY-AFTER-KEY only repeats its rule that no :REPLY enqueue or
reply-sourced :WIRE-IN follows the first key."
  (unless (and (stringp label) (case-of label) (prereq-events-p events))
    (return-from case-reply-end (values nil :arguments)))
  (let* ((at (position :key-begin events :key #'prereq-kind))
         (begin (and at (nth at events)))
         (wanted (first (capture-expectations label))))
    (cond ((null at) (values nil :no-key-begin))
          ((not (and (equal (getf begin :phase) "keys-0")
                     (equal (getf begin :key) (second wanted))))
           (values nil :first-key))
          ((not (typep (getf begin :out) '(integer 0))) (values nil :boundary))
          ((some #'prereq-reply-p (nthcdr (1+ at) events)) (values nil :reply-after-key))
          (t (values (getf begin :out) nil)))))

(defun case-targets (label events barriers output reply-end)
  "Return each capture's :TARGET-OUTPUT in CAPTURE-IDS order and NIL, otherwise
NIL and the first failed requirement.

BARRIERS maps each capture name to the plist the caller read from
<name>.barrier.sexp with the existing safe reader. REPLY-END is CASE-REPLY-END's
result. Requirements, checked in this order:
- LABEL is a keyboard case, EVENTS and BARRIERS have safe shapes, OUTPUT is a
  string and REPLY-END a nonnegative integer (:ARGUMENTS);
- the :CAPTURE events name exactly CAPTURE-IDS in order (:CAPTURE-INVENTORY);
- barrier names are distinct and each names a capture (:BARRIER-INVENTORY);
- each present barrier is EQUAL to its event's :BARRIER (:BARRIER-MISMATCH), its
  target is a nonnegative integer (:TARGET-TYPE) EQL to the event's :OUT
  (:TARGET-OUT-MISMATCH);
- every capture :OUT is a nonnegative integer (:TARGET-TYPE), strictly above
  REPLY-END and the previous one, so each interval is nonempty (:TARGET-ORDER),
  and at most OUTPUT's length (:TARGET-RANGE);
- every capture has a barrier (:MISSING-BARRIER).
Contradictions are reported before a missing barrier."
  (unless (and (stringp label) (case-of label) (prereq-events-p events)
               (prereq-barriers-p barriers) (stringp output) (typep reply-end '(integer 0)))
    (return-from case-targets (values nil :arguments)))
  (let* ((captures (remove :capture events :key #'prereq-kind :test-not #'eq))
         (names (mapcar (lambda (e) (getf e :name)) captures))
         (outs (mapcar (lambda (e) (getf e :out)) captures))
         (given (mapcar #'car barriers)))
    (flet ((refuse (reason) (return-from case-targets (values nil reason))))
      (unless (equal (mapcar (lambda (name) (capture-id label name)) names) (capture-ids label))
        (refuse :capture-inventory))
      (unless (and (= (length given) (length (remove-duplicates given :test #'equal)))
                   (subsetp given names :test #'equal))
        (refuse :barrier-inventory))
      (loop for e in captures for out in outs
            for entry = (assoc (getf e :name) barriers :test #'equal)
            for target = (getf (cdr entry) :target-output)
            when entry
              do (cond ((not (equal (cdr entry) (getf e :barrier))) (refuse :barrier-mismatch))
                       ((not (typep target '(integer 0))) (refuse :target-type))
                       ((not (eql target out)) (refuse :target-out-mismatch))))
      (unless (every (lambda (out) (typep out '(integer 0))) outs) (refuse :target-type))
      (unless (apply #'< reply-end outs) (refuse :target-order))
      (unless (<= (car (last outs)) (length output)) (refuse :target-range))
      (unless (subsetp names given :test #'equal) (refuse :missing-barrier))
      (values outs nil))))
