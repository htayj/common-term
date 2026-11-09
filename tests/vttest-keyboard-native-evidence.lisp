;;;; Native keyboard evidence. Per case one CASE-PROOF read (bytes, prerequisites,
;;;; font, timing) feeds CASE-PIXELS with the same EQ context and verdicts. A missing
;;;; consumer, font proof, scratch or pixel entry is :UNVERIFIED, never :PASS.
;;;; Loaded by vttest-keyboard-native-main.lisp after launch, artifacts and timing.
(in-package #:vttest-keyboard-driver)

(dolist (f '(("vttest-keyboard-native-prereq" . "CASE-PROOF")
             ("vttest-keyboard-context-font" . "MAKE-FONT-PROOF")
             ("vttest-keyboard-png" . "CAPTURE-PIXELS")
             ("vttest-keyboard-pixels" . "CASE-PIXELS")))
  (unless (fboundp (find-symbol (cdr f) '#:vttest-keyboard-driver))
    (load (merge-pathnames (concatenate 'string (car f) ".lisp") *load-truename*))))

(defun proof-of (label dir bytes font-proof)
  "CASE-PROOF's five values TIMING TIMING-REASONS PREREQ PREREQ-REASONS CTX. A
missing consumer is :UNVERIFIED, a signal :FAIL, both with a NIL context. An
unknown TIMING or PREREQ state is :FAIL with a bounded reason."
  (flet ((give-up (state reason) (values state (list reason) state (list reason) nil))
         (known (s reasons)
           (if (member s '(:pass :fail :unverified))
               (values s reasons)
               (values :fail (list (list :case-proof-state (type-of s)))))))
    (if (fboundp 'case-proof)
        (handler-case
            (multiple-value-bind (timing timing-reasons prereq prereq-reasons ctx)
                (case-proof label dir bytes font-proof)
              (multiple-value-bind (timing timing-reasons) (known timing timing-reasons)
                (multiple-value-bind (prereq prereq-reasons) (known prereq prereq-reasons)
                  (values timing timing-reasons prereq prereq-reasons ctx))))
          (error (e) (give-up :fail (list :case-proof-signal (type-of e)))))
        (give-up :unverified '(:missing-consumer :case-proof)))))

(defun pixels-of (label ctx prereq timing scratch)
  "CASE-PIXELS on the same CTX, PREREQ and TIMING: STATUS ENTRIES REASONS. Without
the consumer or an owned SCRATCH directory nothing runs and STATUS is :UNVERIFIED.
Without a context it is :FAIL when PREREQ or TIMING failed or both passed (a
passing proof always carries its context), else :UNVERIFIED."
  (multiple-value-bind (status entries reasons)
      (cond ((not (and (fboundp 'case-pixels) (fboundp 'case-context-p)))
             (values :unverified nil '((:missing-consumer :case-pixels))))
            ((not (case-context-p ctx))
             (values (if (or (member :fail (list prereq timing))
                             (and (eq prereq :pass) (eq timing :pass)))
                         :fail :unverified)
                     nil
                     (list (list :no-context prereq timing))))
            ((not (stringp scratch)) (values :unverified nil '(:no-pixel-scratch)))
            (t (handler-case (case-pixels label ctx prereq timing :scratch scratch)
                 (error (e) (values :fail nil (list (list :case-pixels-signal (type-of e))))))))
    (if (member status '(:pass :fail :unverified))
        (values status entries reasons)
        (values :fail nil (list (list :case-pixels-state (type-of status)))))))

(defun guarded-native-evidence (root font-proof scratch)
  "NATIVE-EVIDENCE's three values plus a list of bounded errors. A signal becomes
no checks, failed-p T, no evidence rows and one (:NATIVE-EVIDENCE-SIGNAL TYPE)."
  (handler-case (multiple-value-bind (checks failed evidence)
                    (native-evidence root font-proof scratch)
                  (values checks failed evidence nil))
    (error (e) (values nil t nil (list (list :native-evidence-signal (type-of e)))))))

(defun check-pixels (identity link entries label capture name)
  "Pixel claim of one receipt check. An identity contradiction fails; otherwise the
entry named NAME must pass, carry LABEL and CAPTURE, and LINK timing must pass."
  (if (eq identity :fail)
      :fail
      (let ((status (if (fboundp 'capture-pixel-status)
                        (handler-case (capture-pixel-status entries name) (error () :fail))
                        :unverified)))
        (cond ((eq status :fail) :fail)
              ((not (eq status :pass)) :unverified)
              ((let ((e (find name entries :key (lambda (x) (getf x :name)) :test #'equal)))
                 (not (and (equal (getf e :label) label) (equal (getf e :id) capture))))
               :fail)
              ((eq link :pass) :pass)
              (t :unverified)))))

(defun native-evidence (root font-proof scratch)
  "Per case: CASE-BYTES, CASE-IDENTITY, one CASE-PROOF, then CASE-PIXELS on that
proof's CTX, PREREQ and TIMING. Values: receipt checks, failed-p, evidence rows."
  (let ((checks nil) (evidence nil) (failed nil))
    (dolist (case +cases+)
      (let ((label (first case)) (dir (native-path root +recorder+)))
        (multiple-value-bind (bytes reasons) (case-bytes label dir)
          (multiple-value-bind (identity originals) (case-identity label dir)
            (multiple-value-bind (timing timing-reasons prereq prereq-reasons ctx)
                (proof-of label dir bytes font-proof)
              (multiple-value-bind (pstatus entries pixel-reasons)
                  (pixels-of label ctx prereq timing scratch)
                (when (member :fail (list bytes identity timing prereq pstatus))
                  (setf failed t))
                (push (list :label label :bytes bytes :identity identity :timing timing
                            :prereq prereq :pixels pstatus
                            :reasons (cap-reasons reasons)
                            :timing-reasons (cap-reasons timing-reasons)
                            :prereq-reasons (cap-reasons prereq-reasons)
                            :pixel-reasons (cap-reasons pixel-reasons))
                      evidence)
                (dolist (original originals)
                  (when (and (not (eq identity :fail))
                             (member (car original) (capture-ids label) :test #'equal)
                             (vttest-area::identifier-p (cdr original)))
                    (let* ((link (linked-timing identity timing))
                           (pixels (check-pixels identity link entries label
                                                 (car original) (cdr original))))
                      (when (eq pixels :fail) (setf failed t))
                      (push (list :scenario label :capture (car original) :name (cdr original)
                                  :pixels pixels :timing link)
                            checks))))))))))
    (values (nreverse checks) failed (nreverse evidence))))
