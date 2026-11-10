(defpackage #:vttest-vt52-navigation
  (:use #:cl) (:export #:vt52-navigation-check))
(in-package #:vttest-vt52-navigation)

;; Pure recorded-output gate, not process authentication or scanout evidence.
;; Markers: reviewed vttest-20251205 vt52.c:33-41,179-205;
;; main.c:278-293,1671-1685; unix_io.c:236-242 (reviews 284-286).
;; Recorder wire semantics: vttest-charsets-recorder.lisp:230-270.
;; Wire-out SOURCE is NIL, OUT is the pre-write offset, not END.
;; Caller must bind CHILD-PID only after matching the actual owned PTY object.
;; START must be taken before the one authorized Return, not from screen text.
;; Complete OUTPUT and chronological EVENTS must be a coherent locked snapshot.
;; Limits: 1 MiB output, 65536 tree conses, 2 MiB aggregate event payload.

(defun octets-p (x)
  (typep x '(vector (unsigned-byte 8))))

(defun bounded-tree-p (root)
  (let ((seen (make-hash-table :test 'eq)) (todo (list root))
        (conses 0) (payload 0))
    (loop while todo for x = (pop todo) do
      (cond
        ((consp x)
         (when (or (gethash x seen) (> (incf conses) 65536))
           (return-from bounded-tree-p nil))
         (setf (gethash x seen) t)
         (push (car x) todo) (push (cdr x) todo))
        ((or (octets-p x) (stringp x))
         (when (or (gethash x seen)
                   (> (incf payload (length x)) 2097152))
           (return-from bounded-tree-p nil))
         (setf (gethash x seen) t))
        ((or (symbolp x) (integerp x)) nil)
        (t (return-from bounded-tree-p nil))))
    t))

(defun plist-p (x)
  (let ((keys nil))
    (loop while (consp x) do
      (unless (and (keywordp (car x)) (consp (cdr x))
                   (not (member (car x) keys)))
        (return-from plist-p nil))
      (push (car x) keys) (setf x (cddr x)))
    (null x)))

(defun vt52-navigation-check (output events start owned-child stage)
  "Validate ordered owned raw output. Return a closed result plist.
MARKERS is a list of (:name keyword :start absolute :end exclusive) plists.
A pending result never authorizes input. Invalid evidence refuses."
  (let ((markers nil))
    (labels ((result (state reasons)
               (list :state state :reasons reasons :stage stage :start start
                     :markers (reverse markers)))
             (refuse (reason) (result :refuse (list reason))))
      (unless (and (member stage '(:first-hold :outer-hold :main-menu))
                   (integerp owned-child) (plusp owned-child)
                   (octets-p output) (<= (length output) 1048576)
                   (integerp start) (<= 0 start (length output)))
        (return-from vt52-navigation-check (refuse :arguments)))
      (unless (bounded-tree-p events)
        (return-from vt52-navigation-check (refuse :event-tree)))
      (let ((tail events) (seq 0) (offset 0))
        (loop while (consp tail) for e = (pop tail) do
          (unless (and (plist-p e) (integerp (getf e :seq))
                       (> (getf e :seq) seq) (keywordp (getf e :kind))
                       (integerp (getf e :out)) (<= 0 (getf e :out))
                       (integerp (getf e :in)) (<= 0 (getf e :in)))
            (return-from vt52-navigation-check (refuse :event-shape)))
          (setf seq (getf e :seq))
          (when (eq (getf e :kind) :wire-out)
            (let ((a (getf e :start)) (b (getf e :end))
                  (bytes (getf e :bytes)))
              (unless (and (integerp a) (integerp b) (= a offset)
                           (= (getf e :out) a) (< a b) (<= b (length output))
                           (octets-p bytes) (= (length bytes) (- b a))
                           (null (getf e :source :missing))
                           (equalp bytes (subseq output a b)))
                (return-from vt52-navigation-check (refuse :wire-range)))
              (when (and (> b start)
                         (not (eql (getf e :child-pid) owned-child)))
                (return-from vt52-navigation-check (refuse :wire-owner)))
              (setf offset b))))
        (unless (and (null tail) (= offset (length output)))
          (return-from vt52-navigation-check (refuse :wire-coverage))))
      (let* ((spec (case stage
                       (:first-hold
                        '((:cleared . "The screen should be cleared, and have a centered")
                          (:rectangle . "rectangle of \"*\"s with \"!\"s on the inside to the")
                          (:sides . "left and right. Only this, and")
                          (:prompt . "Push <RETURN>")))
                       (:outer-hold
                        '((:identify . "Test of terminal response to IDENTIFY command")
                          (:response . "Response was ")
                          (:ok . " -- OK (means VT100 emulating VT52)")
                          (:prompt . "Push <RETURN>")))
                       (:main-menu
                        '((:menu . "Choose test type:")
                          (:vt52 . "7. Test of VT52 mode")
                          (:choice . "Enter choice number (0 - 12): ")))))
             (position start)
             (probe (and (eq stage :outer-hold)
                         (search (map 'vector #'char-code
                                      "You should have to press return to continue:")
                                 output :start2 start))))
        (dolist (entry spec)
          (let* ((pattern (map 'vector #'char-code (cdr entry)))
                 (found (search pattern output :start2 position)))
            (unless found
              (return-from vt52-navigation-check
                (if probe (refuse :extra-probe) (result :pending nil))))
            (when (and probe (eq (car entry) :prompt) (< probe found))
              (return-from vt52-navigation-check (refuse :extra-probe)))
            (setf position (+ found (length pattern)))
            (push (list :name (car entry) :start found :end position) markers)))
        (result :ready nil)))))
