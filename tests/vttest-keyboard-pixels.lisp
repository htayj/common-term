;;;; Keyboard pixels: gate one CASE-PROOF, link each capture to its barrier, then
;;;; compare its PNG exactly with RGBA from the cells model and proven font octets.
;;;; Loads nothing. Load first, in order: vttest-keyboard-driver, -context, -cells,
;;;; -decoder, -png. The native prerequisite file meant to load it is not written.
(in-package #:vttest-keyboard-driver)

(dolist (f '(case-of capture-ids capture-id case-context-p record-proof-step
             capture-pixels pixel-cap vttest-keyboard-cells:capture-ids
             vttest-keyboard-cells:expected-state vttest-keyboard-cells:rasterize-state
             vttest-keyboard-decoder:oracle-rows vttest-screen-oracle:cell-blink))
  (unless (fboundp f) (error "Keyboard pixels needs ~s loaded first" f)))

(defparameter +blink-phase+ t
  "Fixed before any PNG opens: production idles at phase T with no blink cell.")

(defun expected-rgba (label id octets)
  "RGBA8 for capture ID of LABEL, or NIL and a reason. Blink cells are refused."
  (let ((state (vttest-keyboard-cells:expected-state label id)))
    (if (some #'vttest-screen-oracle:cell-blink (getf state :cells))
        (values nil :expected-blink-cells)
        (vttest-keyboard-cells:rasterize-state
         state (lambda (code) (vttest-keyboard-decoder:oracle-rows octets code))
         :blink-phase +blink-phase+))))

(defun linked-captures (label ctx)
  "CTX's capture events when they are exactly CAPTURE-IDS in order (both tables
agreeing), each with a barrier EQUAL to its :BARRIER whose :TARGET-OUTPUT is EQL
its :OUT. Values: captures or NIL, status, reason."
  (let ((ids (capture-ids label)) (missing nil)
        (captures (remove :capture (case-context-events ctx)
                          :key (lambda (e) (getf e :kind)) :test-not #'eq)))
    (unless (equal ids (vttest-keyboard-cells:capture-ids label))
      (return-from linked-captures (values nil :fail :capture-id-tables)))
    (unless (equal ids (mapcar (lambda (e) (capture-id label (getf e :name))) captures))
      (return-from linked-captures (values nil :fail :capture-inventory)))
    (dolist (e captures)
      (let ((b (assoc (getf e :name) (case-context-barriers ctx) :test #'equal)))
        (cond ((null b) (setf missing (or missing (getf e :name))))
              ((not (and (equal (cdr b) (getf e :barrier))
                         (eql (getf (cdr b) :target-output) (getf e :out))))
               (return-from linked-captures (values nil :fail (list :barrier-link (car b))))))))
    (if missing
        (values nil :unverified (list :missing-barrier missing))
        (values captures :pass nil))))

(defun capture-entry (label ctx event scratch)
  (let* ((name (getf event :name)) (id (capture-id label name)))
    (multiple-value-bind (status reasons hash raw)
        (multiple-value-bind (expected why)
            (handler-case (expected-rgba label id (case-context-oracle-octets ctx))
              (error (e) (values nil (list :expected (princ-to-string e)))))
          (cond (expected (capture-pixels (case-context-dir ctx) name expected scratch))
                ((eq why :expected-blink-cells) (values :unverified (list why)))
                (t (values :fail (list why)))))
      (list :label label :name name :id id :pixels status :reasons reasons
            :sha256 hash :raw raw))))

(defun case-pixels (label ctx prereq timing &key (scratch "/tmp/"))
  "Pixels of LABEL on CTX. Nothing is opened unless PREREQ and TIMING from one
CASE-PROOF both pass. Values: status, entries in capture order, reasons."
  (let ((octets (and (case-context-p ctx) (case-context-oracle-octets ctx))))
    (flet ((refuse (status &rest why) (return-from case-pixels (values status nil why))))
      (cond ((or (eq prereq :fail) (eq timing :fail)) (refuse :fail :proof prereq timing))
            ((not (and (case-context-p ctx) (case-of label)
                       (equal label (case-context-label ctx))))
             (refuse :fail :context))
            ((not (and (eq prereq :pass) (eq timing :pass)))
             (refuse :unverified :proof prereq timing))
            ((not (and (typep octets '(vector (unsigned-byte 8))) (plusp (length octets))))
             (refuse :fail :oracle-octets)))))
  (record-proof-step :pixels ctx)
  (multiple-value-bind (captures status reason)
      (handler-case (linked-captures label ctx)
        (error (e) (values nil :fail (list :links (princ-to-string e)))))
    (unless captures (return-from case-pixels (values status nil (list reason))))
    (let* ((entries (mapcar (lambda (e) (capture-entry label ctx e scratch)) captures))
           (bad (remove :pass entries :key (lambda (x) (getf x :pixels)))))
      (values (cond ((find :fail bad :key (lambda (x) (getf x :pixels))) :fail)
                    (bad :unverified)
                    (t :pass))
              entries
              (pixel-cap (mapcar (lambda (x) (list (getf x :id) (getf x :pixels))) bad))))))

(defun capture-pixel-status (entries name)
  "Pixel status of capture NAME in ENTRIES; :UNVERIFIED when absent."
  (let ((e (find name entries :key (lambda (x) (getf x :name)) :test #'equal)))
    (if (member (getf e :pixels) '(:pass :fail)) (getf e :pixels) :unverified)))

(defun pixel-aggregate (checks &optional count)
  "Entries of both cases: :FAIL on any failure; :PASS only when all pass and they
are exactly every case's captures, COUNT of them when given; else :UNVERIFIED."
  (let ((wanted (loop for (l) in +cases+ append (mapcar (lambda (i) (cons l i)) (capture-ids l))))
        (seen (mapcar (lambda (k) (cons (getf k :label) (getf k :id))) checks)))
    (cond ((find :fail checks :key (lambda (k) (getf k :pixels))) :fail)
          ((and (every (lambda (k) (eq (getf k :pixels) :pass)) checks)
                (= (length seen) (length wanted) (or count (length wanted)))
                (= (length seen) (length (remove-duplicates seen :test #'equal)))
                (subsetp seen wanted :test #'equal))
           :pass)
          (t :unverified))))
