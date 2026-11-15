;;;; Fixed submission budgets and offline decision controls. No native launches.
(defpackage #:lispterm.latency.budgets
  (:use #:cl #:lispterm.latency #:lispterm.latency.tokens)
  (:export #:evaluate-evidence #:budget-problems #:recorder-admission #:run
           #:+dispatch-write-limit-ns+ #:+parse-commit-limit-ns+))
(in-package #:lispterm.latency.budgets)

(defconstant +dispatch-write-limit-ns+ 2000000)
(defconstant +parse-commit-limit-ns+ 20000000)

(defun budget-problems (dispatch-p95 parse-p95)
  "Fixed, inclusive engineering limits. Calibration never raises them."
  (append (unless (and (integerp dispatch-p95) (<= 0 dispatch-p95 +dispatch-write-limit-ns+))
            '(:dispatch-write-budget))
          (unless (and (integerp parse-p95) (<= 0 parse-p95 +parse-commit-limit-ns+))
            '(:parse-commit-budget))))

(defun recorder-admission (receipt events)
  "Normalize CALL-WITH-RECORDER's drained, trace-checked snapshot receipt.
A recorder-local :SEALED receipt alone lacks the trace-boundary check."
  (let ((snapshot (getf receipt :snapshot)))
    (if (and (eq :complete (getf receipt :state))
             (null (getf receipt :trace-failure :missing))
             (eq :sealed (getf snapshot :state))
             (eq t (getf snapshot :closed-p))
             (eql 0 (getf snapshot :active))
             (eql (length events) (getf snapshot :event-count)))
        :complete
        :incomplete)))

(defun audit-report-p (report skipped-p)
  "Admit the separate native fixture's four-token copy control report.
This checks its receipt, not independent pixel authentication. The native
fixture must produce this report from actual DMA readback and skip copying."
  (let* ((matching (getf report :matching))
         (hashes (getf report :audit-hashes))
         (completeness (getf report :audit-completeness))
         (disagreements
           (loop for row in hashes
                 for index = (getf row :index)
                 for before = (getf row :before)
                 for committed = (getf row :committed)
                 for settled = (getf row :settled)
                 unless (and (eql committed settled) (not (eql before committed)))
                   collect index)))
    (and (eq :nontimed-native-audit (getf report :claim))
         (eq t (getf report :frozen)) (eql 0 (getf report :error-count))
         (eq :complete (getf report :recorder-admission))
         (equal '(:dma) (getf report :observed-paths))
         (equal '(1) (getf report :observed-scales))
         (eql 4 (getf report :count))
         (eq :complete (getf matching :outcome))
         (null (getf matching :problems)) (eql 4 (getf matching :matched))
         (eql 4 (getf completeness :matched))
         (eql 4 (getf completeness :complete))
         (null (getf completeness :missing))
         (equal disagreements (getf completeness :disagreements))
         (= 4 (length hashes))
         (equal '(0 1 2 3) (sort (mapcar (lambda (row) (getf row :index)) hashes) #'<))
         (every (lambda (row)
                  (every (lambda (key) (integerp (getf row key)))
                         '(:before :committed :settled))) hashes)
         (equal disagreements (getf report :audit-disagreements))
         (if skipped-p
             (equal '(0 1 2 3) (sort (copy-list disagreements) #'<))
             (null disagreements)))))

(defun evaluate-evidence (events probes nonce metadata calibration audit-real audit-skip
                          &key injections recorder-admission)
  "Re-match full readback-free stage evidence, then evaluate the frozen idle case.
METADATA, CALIBRATION and audit reports use the raw fixture's existing plists.
Return :PASS only after admission and both budgets; missing proof is :REJECTED.
This does not claim physical display latency or authenticate arbitrary receipts."
  (multiple-value-bind (samples matching)
      (match-run events :count 70 :slots 24 :probes probes :input-path :display
                        :nonce nonce :injections injections)
    (let ((problems nil) (dispatch nil) (parse nil))
      (flet ((require-proof (value reason) (unless value (push reason problems))))
        ;; Normalize the final sealed recorder receipt at the call site.
        ;; Missing, failed and in-flight snapshots cannot enter acceptance.
        (require-proof (eq :complete recorder-admission) :recorder-snapshot)
        (require-proof (and (eq :complete (getf matching :outcome))
                            (eql 70 (getf matching :matched))
                            (null (getf matching :problems))) :incomplete-token-chain)
        (require-proof (and (eq t (getf metadata :frozen))
                            (eql 0 (getf metadata :error-count))) :source-or-host)
        (require-proof (and (eq :submission-stage-samples (getf metadata :claim))
                            (equal '(:dma) (getf metadata :observed-paths))
                            (equal '(1) (getf metadata :observed-scales))
                            (equal '(:rows 37 :columns 100) (getf metadata :grid))
                            (member '(800 600) (getf metadata :copy-dimensions) :test #'equal)
                            (eql 70 (getf metadata :count))
                            (eql 20 (getf metadata :warm-up))
                            (eql 50 (getf metadata :measured))) :wrong-case)
        (require-proof (and (getf calibration :clock)
                            (null (clock-problems (getf calibration :clock)))
                            (integerp (getf (getf calibration :wake-jitter-ns) :p95))
                            (<= 0 (getf (getf calibration :wake-jitter-ns) :p95) 1000000))
                       :calibration)
        (require-proof (audit-report-p audit-real nil) :positive-copy-audit)
        (require-proof (audit-report-p audit-skip t) :skipped-copy-audit)
        (require-proof (notany (lambda (event) (getf (cddr event) :audit)) events)
                       :timing-has-pixel-readback)
        (require-proof (every (lambda (sample)
                                (and (eq :matched (sample-status sample))
                                     (eq :dma (sample-path sample)))) samples) :sample-path)
        (when (null problems)
          (let ((measured (subseq samples 20)))
            (setf dispatch (summarize
                            (map 'list (lambda (s) (- (sample-write s)
                                                     (lispterm.latency::sample-dispatch s))) measured))
                  parse (summarize
                         (map 'list (lambda (s) (- (sample-commit s) (sample-parsed s))) measured))))))
      (let ((budget (when dispatch (budget-problems (getf dispatch :p95) (getf parse :p95)))))
        (list :outcome (cond (problems :rejected) (budget :fail) (t :pass))
              :problems (nreverse problems) :budget-problems budget :matching matching
              :dispatch-to-write-ns dispatch :parse-to-commit-ns parse
              :limits-ns (list :dispatch-write +dispatch-write-limit-ns+
                               :parse-commit +parse-commit-limit-ns+ :wake-jitter 1000000))))))

;;; Sanitized valid causal evidence. Each sample is separate, preserving arrival
;;; order even after a 40ms delay. These are not measurements of production.
(defun fixture (&key input-delay render-delay omit-token)
  (let ((events nil) (probes nil))
    (dotimes (i 70)
      (let* ((base (+ 1000000000 (* i 100000000)))
             (delay (or input-delay 0)) (render (or render-delay 0))
             (token (unless (eql omit-token i) (list i))))
        (dolist (event (list (list :key-dispatch base :pressed t)
                             (list :event-dequeue (+ base 10) :kind :key-event)
                             (list :input-enqueue (+ base 20) :bytes 1)
                             (list :pty-write (+ base 1000000 delay) :accepted 1)
                             (list :pty-read (+ base 2000000 delay) :bytes 20)
                             (list :feed-done (+ base 2100000 delay) :tokens token)
                             (list :draw-start (+ base 2200000 delay render) :kind :frame :tokens token)
                             (list :copy (+ base 3000000 delay render) :path :dma :buffer i)
                             (list :commit (+ base 4000000 delay render) :path :dma :buffer i :generation (1+ i))))
          (push event events))
        (push (list i (key-byte i) (+ base 1100000 delay) (+ base 1300000 delay)
                    (+ base 1200000 delay) "BC") probes)))
    (values (nreverse events) (nreverse probes))))

(defun delay-evidence (events probes kind)
  "Copy evidence and add 40ms downstream of input write or token parse."
  (let ((copy (copy-tree events)) (answers (copy-tree probes)))
    (dolist (event copy)
      (when (member (first event)
                    (if (eq kind :input)
                        '(:pty-write :pty-read :feed-done :draw-start :copy :commit)
                        '(:draw-start :copy :commit)))
        (incf (second event) 40000000)))
    (when (eq kind :input)
      (dolist (probe answers)
        (incf (third probe) 40000000)
        (incf (fourth probe) 40000000)
        (incf (fifth probe) 40000000)))
    (values copy answers)))

(defun fixture-metadata ()
  (list :claim :submission-stage-samples :frozen t :error-count 0
        :observed-paths '(:dma) :observed-scales '(1) :grid '(:rows 37 :columns 100)
        :copy-dimensions '((800 600)) :count 70 :warm-up 20 :measured 50))
(defun fixture-calibration ()
  (list :clock '(:resolution-ns 1 :backwards 0 :smallest-step-ns 20)
        :wake-jitter-ns '(:p95 100000)))
(defun fixture-audit (&optional skipped)
  (list :claim :nontimed-native-audit :frozen t :error-count 0
        :recorder-admission :complete
        :observed-paths '(:dma) :observed-scales '(1) :count 4
        :matching '(:outcome :complete :matched 4 :problems nil)
        :audit-completeness (list :matched 4 :complete 4 :missing nil
                                  :disagreements (when skipped '(0 1 2 3)))
        :audit-hashes (loop for i below 4
                            collect (list :index i :before 1
                                          :committed (if skipped 1 2)
                                          :settled (if skipped 1 2)))
        :audit-disagreements (when skipped '(0 1 2 3))))

(defun fixture-injections (events)
  (loop for event in events
        when (eq :key-dispatch (first event))
          collect (list (floor (- (second event) 1000000000) 100000000)
                        (- (second event) 100) (+ (second event) 100))))

(defun run ()
  (let ((checks 0) (failures nil))
    (labels ((check (name value)
               (incf checks) (unless value (push name failures))
               (format t "~S~%" (list :check name :pass (and value t))))
             (evaluate (events probes &optional (meta (fixture-metadata))
                          (cal (fixture-calibration)) (real (fixture-audit)) (skip (fixture-audit t)))
               (evaluate-evidence events probes "BC" meta cal real skip
                                  :recorder-admission :complete
                                  :injections (fixture-injections events)))
             (rejects (result reason)
               (and (eq :rejected (getf result :outcome))
                    (member reason (getf result :problems)))))
      (multiple-value-bind (events receipt)
          (call-with-recorder (make-recorder "RC" 4)
                              (lambda () (lispterm.trace:trace-event :admission-control :value 1)))
        (check :actual-drained-recorder-admitted
               (and (= 1 (length events)) (eq :complete (recorder-admission receipt events))))
        (dolist (change '((:state :failed) (:state :in-flight)
                          (:trace-failure t) (:snapshot nil)))
          (let ((copy (copy-tree receipt)))
            (setf (getf copy (first change)) (second change))
            (check (list :bad-recorder-receipt change)
                   (not (eq :complete (recorder-admission copy events))))))
        (dolist (change '((:state :unsealed) (:closed-p nil) (:active 1) (:event-count 999)))
          (let ((copy (copy-tree receipt)))
            (setf (getf (getf copy :snapshot) (first change)) (second change))
            (check (list :bad-sealed-snapshot change)
                   (not (eq :complete (recorder-admission copy events)))))))
      (multiple-value-bind (events sealed) (recorded-events (make-recorder "RS" 4))
        (check :recorder-local-seal-is-not-trace-boundary-proof
               (not (eq :complete (recorder-admission sealed events)))))
      (check :actual-recorder-failure-cannot-yield-admission
             (handler-case
                 (progn
                   (call-with-recorder
                    (make-recorder "RF" 4 :audit (lambda (&rest args)
                                                   (declare (ignore args))
                                                   (error "deliberate audit failure")))
                    (lambda () (lispterm.trace:trace-event :copy :store nil)))
                   nil)
               (measurement-error () t)))
      (multiple-value-bind (events probes) (fixture)
        (check :sanitized-valid-case (eq :pass (getf (evaluate events probes) :outcome)))
        (dolist (status '(nil :failed :in-flight))
          (let ((result (evaluate-evidence events probes "BC" (fixture-metadata)
                                           (fixture-calibration) (fixture-audit) (fixture-audit t)
                                           :injections (fixture-injections events)
                                           :recorder-admission status)))
            (check (list :recorder-snapshot-rejected status)
                   (and (eq :rejected (getf result :outcome))
                        (equal '(:recorder-snapshot) (getf result :problems))))))
        (check :limits-inclusive (null (budget-problems 2000000 20000000)))
        (check :input-one-ns-over (equal '(:dispatch-write-budget) (budget-problems 2000001 20000000)))
        (check :render-one-ns-over (equal '(:parse-commit-budget) (budget-problems 2000000 20000001)))
        ;; Recorded p95 values from validated baseline124, not sanitized timings.
        (check :old-recorded-baseline-rejected-for-both-delays
               (equal '(:dispatch-write-budget :parse-commit-budget)
                      (budget-problems 5963530 62852372)))
        (check :missing-probe (rejects (evaluate events (rest probes)) :incomplete-token-chain))
        (check :missing-commit (rejects (evaluate (remove :commit events :key #'first) probes)
                                         :incomplete-token-chain))
        (check :wrong-run (let ((copy (copy-tree probes)))
                            (setf (sixth (first copy)) "STALE")
                            (rejects (evaluate events copy) :incomplete-token-chain)))
        (check :unstable-calibration (let ((cal (fixture-calibration)))
                                       (setf (getf cal :wake-jitter-ns) '(:p95 1000001))
                                       (rejects (evaluate events probes (fixture-metadata) cal) :calibration)))
        (check :missing-positive-audit
               (rejects (evaluate events probes (fixture-metadata) (fixture-calibration) nil)
                        :positive-copy-audit))
        (check :skip-copy-control-must-disagree
               (rejects (evaluate events probes (fixture-metadata) (fixture-calibration)
                                  (fixture-audit) (fixture-audit)) :skipped-copy-audit))
        ;; Missing evidence is never a successful skipped-copy perturbation.
        (dolist (skipped '(nil t))
          (dotimes (index 4)
            (dolist (key '(:before :committed :settled))
              (let ((audit (fixture-audit skipped)))
                (remf (nth index (getf audit :audit-hashes)) key)
                (check (list :missing-audit-hash skipped index key)
                       (rejects (evaluate events probes (fixture-metadata)
                                          (fixture-calibration)
                                          (if skipped (fixture-audit) audit)
                                          (if skipped audit (fixture-audit t)))
                                (if skipped :skipped-copy-audit :positive-copy-audit)))))))
        (check :unfrozen-source (let ((meta (fixture-metadata)))
                                  (setf (getf meta :frozen) nil)
                                  (rejects (evaluate events probes meta) :source-or-host)))
        (check :host-error (let ((meta (fixture-metadata)))
                             (setf (getf meta :error-count) 1)
                             (rejects (evaluate events probes meta) :source-or-host)))
        (check :wrong-grid (let ((meta (fixture-metadata)))
                             (setf (getf meta :grid) '(:rows 24 :columns 80))
                             (rejects (evaluate events probes meta) :wrong-case)))
        (check :readback-excluded (let ((copy (copy-tree events)))
                                    (setf (getf (cddr (find :copy copy :key #'first)) :audit) '((1 . 42)))
                                    (rejects (evaluate copy probes) :timing-has-pixel-readback))))
      (dolist (kind '(:input :render))
        ;; Delay copied evidence, not the production scheduler.
        (multiple-value-bind (original probes) (fixture)
          (multiple-value-bind (events answers) (delay-evidence original probes kind)
            (let ((result (evaluate events answers)))
              (check (list :synthetic-40ms kind)
                     (and (eq :fail (getf result :outcome))
                          (equal (list (if (eq kind :input) :dispatch-write-budget :parse-commit-budget))
                                 (getf result :budget-problems))
                          (eq :pass (getf (evaluate original probes) :outcome))))))))
      (multiple-value-bind (events probes) (fixture :omit-token 30)
        (check :unmatched-token (rejects (evaluate events probes) :incomplete-token-chain)))
      (format t "~S~%" (list :controls checks :failures (nreverse failures)
                              :scope :offline-synthetic-measurement-decisions))
      (null failures))))
