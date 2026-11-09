;;;; Independent cleanup and timing integration; pixels are linked claims.
(in-package #:vttest-keyboard-driver)

(defun native-cleanup (root backend record)
  (if (null root) (values (cleanup-status record) '(:no-run-directory) nil)
      (handler-case
          (multiple-value-bind (status reasons projection)
              (consume-gate-cleanup root backend
                                    (cleanup-read root "GATE-LAUNCH.sexp")
                                    (cleanup-read root "GATE-EXIT.sexp"))
            (let ((claimed (getf record :cleanup)))
              (when (and claimed projection
                         (cleanup-projection-conflict-p claimed projection))
                (setf status :fail reasons (cons :cleanup-projection-mismatch reasons)))
              (when (eq (cleanup-status record) :fail) (setf status :fail)))
            (values status reasons projection))
        (error () (values :fail '(:malformed-launch-record) nil)))))

(defun check-for (record label capture)
  (find-if (lambda (k) (and (equal (getf k :scenario) label) (equal (getf k :capture) capture)))
           (getf record :checks)))

(defun pixel-status (identity check original &optional timing)
  "Native-main's pixel claim, trusted only when linked: identity :PASS, the check names
ORIGINAL, and TIMING-STATUS over the recomputed case TIMING is :PASS. The summary
cannot recompute pixels without the run's live font proof."
  (cond ((eq identity :fail) :fail)
        ((null original) :unverified)
        ((null check) :unverified)
        ((not (equal (getf check :name) original)) :fail)
        ((eq (getf check :pixels) :fail) :fail)
        ((and (eq (getf check :pixels) :pass)
              (eq (timing-status identity check original timing) :pass))
         :pass)
        (t :unverified)))

(defun summarize-native (manifest record process-exit &optional root)
  "Reconcile cleanup and timing independently; pixels pass only as linked claims."
  (let ((receipt-error nil) (dir (and root (concatenate 'string root +recorder+))))
    (handler-case (when record (validate-receipt record (getf manifest :backend) root))
      (error (e) (setf receipt-error (princ-to-string e) record nil)))
    (multiple-value-bind (cleanup cleanup-reasons cleanup-projection)
        (native-cleanup root (getf manifest :backend) record)
      (let ((observations nil) (cases nil)
            (passed 0) (failed 0) (unverified 0) (captured 0))
      (dolist (s (getf manifest :scenarios))
        (let ((label (getf s :recorder-label)))
          (multiple-value-bind (bytes errors)
              (if dir (case-bytes label dir) (values :unverified '(:no-run-directory)))
            (multiple-value-bind (identity originals)
                (if dir (case-identity label dir) :unverified)
              (incf captured (count-if (lambda (c) (handler-case (safe-image-p (artifact dir (cdr c) ".png")) (error () nil)))
                                       originals))
              (multiple-value-bind (case-state timing-errors) (case-timing label dir bytes +timing-scale+)
              (push (list :label label :bytes bytes :identity identity :timing case-state
                          :byte-errors (subseq errors 0 (min 8 (length errors)))
                          :timing-errors (subseq timing-errors 0 (min 8 (length timing-errors))))
                    cases)
              (dolist (c (getf s :captures))
                (let* ((id (getf c :id)) (check (check-for record label id))
                       (pixels (pixel-status identity check
                                             (cdr (assoc id originals :test #'equal)) case-state))
                       (timing (timing-status identity check
                                              (cdr (assoc id originals :test #'equal)) case-state))
                       (states (list pixels bytes timing cleanup)))
                  (cond ((member :fail states) (incf failed))
                        ((member :unverified states) (incf unverified))
                        (t (incf passed)))
                  (push (list :scenario (getf s :id) :capture id
                              :fresh (and (eql process-exit 0) record
                                          (eq (getf record :status) :pass)
                                          (eq cleanup :pass) (eq bytes :pass) t)
                              :fixture nil :pixels pixels :bytes bytes :timing timing)
                        observations))))))))
      (values (nreverse observations)
              (list :native-exit process-exit :native-record record :receipt-error receipt-error
                    :native-failed (and (or (and process-exit (not (member process-exit '(0 3))))
                                            receipt-error
                                            (and record (eq (getf record :status) :fail))
                                            (eq cleanup :fail))
                                        t)
                    :consumer-integration :required :cleanup cleanup
                    :cleanup-reasons cleanup-reasons :cleanup-projection cleanup-projection
                    :cases (nreverse cases)
                    :counts (list :required (getf manifest :required-count) :captured captured
                                  :passed passed :failed failed :unverified unverified)))))))

