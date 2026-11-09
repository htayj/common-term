;;;; Keyboard case prerequisites, then timing, over one LOAD-CASE-CONTEXT read.
;;;; Load after vttest-area.lisp (vttest-check, vttest-keyboard-driver with
;;;; artifacts and timing). The caller builds FONT-PROOF once per run with
;;;; MAKE-FONT-PROOF (vttest-keyboard-context-font.lisp). Pixels are separate.
(unless (and (find-package '#:vttest-check) (find-package '#:vttest-keyboard-driver))
  (error "Load vttest-area.lisp before ~a." *load-truename*))
(in-package #:vttest-keyboard-driver)

(dolist (f '(("vttest-keyboard-da" . "OWN-DA-REPLY-P")
             ("vttest-keyboard-prerequisites" . "CASE-TARGETS")
             ("vttest-keyboard-context" . "LOAD-CASE-CONTEXT")))
  (unless (fboundp (find-symbol (cdr f) '#:vttest-keyboard-driver))
    (load (merge-pathnames (concatenate 'string (car f) ".lisp") *load-truename*))))
(unless (find-package '#:vttest-keyboard-row-output)
  (load (merge-pathnames "vttest-keyboard-row-output.lisp" *load-truename*)))

(defun cap-reasons (reasons)
  "At most 8 reasons, each a keyword or a proper list headed by one. Other data
is wrapped as (:REASON X): a number, symbol, character or string as itself,
anything else, which may be circular, as its TYPE-OF."
  (loop for r in (if (ignore-errors (list-length reasons)) reasons (list reasons))
        repeat 8
        collect (cond ((or (keywordp r)
                           (and (consp r) (keywordp (car r)) (ignore-errors (list-length r))))
                       r)
                      ((typep r '(or number symbol character string)) (list :reason r))
                      (t (list :reason (type-of r))))))

(defun font-result (font-proof)
  "STATE REASONS OCTETS of the font closure. An absent consumer or an explicit
:UNVERIFIED is :UNVERIFIED; a signal, a malformed :PASS (reasons, or octets
missing, empty or not (UNSIGNED-BYTE 8)) or an unknown state is :FAIL. A valid
:PASS returns a copy of its octets."
  (unless (functionp font-proof)
    (return-from font-result (values :unverified '((:font :no-font-consumer)) nil)))
  (multiple-value-bind (state reasons octets)
      (handler-case (funcall font-proof)
        (error (e) (values :fail (list (list :signal (type-of e))) nil)))
    (let ((reasons (mapcar (lambda (r) (list :font r)) (cap-reasons reasons))))
      (cond ((and (eq state :pass) (null reasons)
                  (typep octets '(vector (unsigned-byte 8))) (plusp (length octets)))
             (values :pass nil (copy-seq octets)))
            ((eq state :pass) (values :fail '((:font :malformed-pass)) nil))
            ((eq state :fail) (values :fail (or reasons '((:font :fail))) nil))
            ((eq state :unverified) (values :unverified (or reasons '((:font :unverified))) nil))
            (t (values :fail (list (list :font :state (type-of state))) nil))))))

(defun timing-result (timing label dir events output barriers)
  "TIMING's STATE and capped REASONS; a signal or malformed answer fails, and an
:UNVERIFIED without reasons gets (:TIMING-UNVERIFIED)."
  (multiple-value-bind (state reasons)
      (handler-case (funcall timing label dir events output +timing-scale+ :barriers barriers)
        (error (e) (values :fail (list (list :timing-signal (type-of e))))))
    (if (or (member state '(:fail :unverified)) (and (eq state :pass) (null reasons)))
        (values state (or (cap-reasons reasons)
                          (and (eq state :unverified) '(:timing-unverified))))
        (values :fail (list (list :timing-result state))))))

(defun case-proof (label dir bytes font-proof &key (timing #'context-timing))
  "Return TIMING TIMING-REASONS PREREQ PREREQ-REASONS CTX for LABEL under DIR.
BYTES is CASE-BYTES' state. Steps stop at the first nonpass, whose state and
reasons become both the prerequisite and the timing result (TIMING not called):
 BYTES :PASS; one LOAD-CASE-CONTEXT; :OWN-DA OWN-DA-REPLY-P (ESC[?1;2c before
 the first key, :OUT progression); :FONT the FONT-PROOF closure, whose octets
 then fill ORACLE-OCTETS; :REPLY-END CASE-REPLY-END, the first :KEY-BEGIN :OUT;
 :TARGETS CASE-TARGETS (capture :OUTs strictly increasing, each EQL its one
 barrier :TARGET-OUTPUT); :ROW CASE-ROW-OUTPUT-ERRORS, intervals from the reply
 end, then each previous target. All passing gives PREREQ :PASS and TIMING's own
 result on the same events, output and :BARRIERS at +TIMING-SCALE+. Missing
 input or barrier is :UNVERIFIED, a contradiction :FAIL. Each step is traced on
 entry with the same CTX."
  (let ((ctx nil))
    (flet ((stop (state reasons)
             (let ((reasons (cap-reasons reasons)))
               (return-from case-proof (values state reasons state reasons ctx))))
           (enter (name) (record-proof-step name ctx)))
      (unless (eq bytes :pass)
        (stop (if (eq bytes :fail) :fail :unverified) (list (list :bytes bytes))))
      (multiple-value-bind (loaded state reasons) (load-case-context label dir)
        (setf ctx loaded)
        (unless (eq state :pass) (stop (if (eq state :fail) :fail :unverified) reasons)))
      (unless (and (case-context-p ctx) (equal (case-context-label ctx) label)
                   (equal (case-context-dir ctx) dir) (null (case-context-oracle-octets ctx)))
        (stop :fail '(:context-shape)))
      (let ((events (case-context-events ctx)) (output (case-context-output ctx))
            (barriers (case-context-barriers ctx)))
        (handler-case
            (progn
              (enter :own-da)
              (multiple-value-bind (ok why) (own-da-reply-p label events output)
                (unless (eq ok t) (stop :fail (list (list :own-da why)))))
              (enter :font)
              (multiple-value-bind (state reasons octets) (font-result font-proof)
                (unless (eq state :pass) (stop state reasons))
                (setf (case-context-oracle-octets ctx) octets))
              (enter :reply-end)
              (multiple-value-bind (reply-end why) (case-reply-end label events)
                (unless reply-end (stop :fail (list (list :reply-end why))))
                (enter :targets)
                (multiple-value-bind (targets why)
                    (case-targets label events barriers output reply-end)
                  (unless targets
                    (stop (if (eq why :missing-barrier) :unverified :fail)
                          (list (list :targets why))))
                  (enter :row)
                  (let ((errors (vttest-keyboard-row-output:case-row-output-errors
                                 label output reply-end targets)))
                    (when errors (stop :fail (mapcar (lambda (e) (cons :row e)) errors)))))))
          (error (e) (stop :fail (list (list :prereq-signal (type-of e))))))
        (enter :timing)
        (multiple-value-bind (state reasons)
            (timing-result timing label dir events output barriers)
          (values state reasons :pass nil ctx))))))
