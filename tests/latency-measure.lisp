;;;; Measurement side of the latency trace: clock checks, statistics, the
;;;; recorder behind LISPTERM.TRACE:*TRACE*, and the token-to-commit matcher.
;;;; Load after the lispterm system and tests/latency-tokens.lisp.
(defpackage #:lispterm.latency
  (:use #:cl #:lispterm.latency.tokens)
  (:import-from #:lispterm.trace #:monotonic-ns)
  (:export #:measurement-error #:clock-resolution-ns #:clock-report #:clock-problems
           #:percentile #:summarize #:range-verdict
           #:+wake-jitter-ceiling-ns+ #:measure-wake-jitter #:calibration-outcome
           #:make-recorder #:recorder-hook #:recorded-events #:call-with-recorder
           #:screen-token-indexes #:grid-token-indexes #:screen-ready-p
           #:match-run #:sample-intervals #:run-summary #:audit-agreement
           #:pixel-rectangle-hash #:shm-audit
           #:sample-index #:sample-status #:sample-problem #:sample-commit #:sample-parsed
           #:sample-pty-read #:sample-write #:sample-enqueue #:sample-probe-read
           #:sample-probe-write #:sample-reference #:sample-intervening #:sample-deferred
           #:sample-coalesced-p #:sample-commit-generation #:sample-path))
(in-package #:lispterm.latency)

(define-condition measurement-error (error)
  ((detail :initarg :detail :reader measurement-error-detail))
  (:report (lambda (condition stream)
             (format stream "Latency measurement: ~A" (measurement-error-detail condition)))))

;;; Clock

(defun clock-resolution-ns ()
  "Resolution the kernel reports for CLOCK_MONOTONIC, in nanoseconds."
  (sb-alien:with-alien ((time (array sb-alien:long 2)))
    (unless (zerop (sb-alien:alien-funcall
                    (sb-alien:extern-alien "clock_getres"
                                           (function sb-alien:int sb-alien:int
                                                     (* (array sb-alien:long 2))))
                    sb-unix:clock-monotonic (sb-alien:addr time)))
      (error 'measurement-error :detail "clock_getres failed"))
    (+ (* 1000000000 (sb-alien:deref time 0)) (sb-alien:deref time 1))))

(defun clock-report (&key (clock #'monotonic-ns) (resolution (clock-resolution-ns)) (reads 20000))
  "Resolution and consecutive-read deltas of CLOCK, for the run header."
  (let ((backwards 0) (smallest nil) (previous (funcall clock)))
    (dotimes (i reads)
      (let* ((now (funcall clock)) (delta (- now previous)))
        (cond ((minusp delta) (incf backwards))
              ((and (plusp delta) (or (null smallest) (< delta smallest))) (setf smallest delta)))
        (setf previous now)))
    (list :resolution-ns resolution :reads reads :backwards backwards
          :smallest-step-ns smallest :implementation (lisp-implementation-version))))

(defun clock-problems (report)
  "Reasons REPORT's clock cannot time sub-millisecond stages; NIL when it can."
  (let ((problems '()))
    (unless (<= (getf report :resolution-ns) 1000)
      (push "reported resolution is coarser than one microsecond" problems))
    (when (plusp (getf report :backwards)) (push "clock went backwards" problems))
    (unless (and (getf report :smallest-step-ns) (<= (getf report :smallest-step-ns) 100000))
      (push "no step below 100 microseconds was observed" problems))
    (nreverse problems)))

;;; Statistics

(defun percentile (values fraction)
  "Nearest-rank percentile of VALUES, a nonempty sequence of integers."
  (let* ((sorted (sort (coerce values 'vector) #'<)) (count (length sorted)))
    (when (zerop count) (error 'measurement-error :detail "no samples"))
    (unless (every #'integerp sorted)
      (error 'measurement-error :detail "a sample is not an integer nanosecond count"))
    (aref sorted (max 0 (1- (ceiling (* fraction count)))))))

(defun summarize (values)
  (let ((values (coerce values 'list)))
    (list :count (length values) :median (percentile values 1/2) :p95 (percentile values 95/100)
          :minimum (reduce #'min values) :maximum (reduce #'max values)
          :negative (count-if #'minusp values))))

(defun range-verdict (lower upper threshold)
  "Verdict for an interval known only to lie between LOWER and UPPER."
  (cond ((<= upper threshold) :pass) ((> lower threshold) :fail) (t :inconclusive)))

;;; Calibration. The ceiling is fixed: no threshold is derived from measured jitter.

(defconstant +wake-jitter-ceiling-ns+ 1000000)

(defun measure-wake-jitter (&key (count 200) (interval 0.002))
  "Overshoot in nanoseconds of COUNT sleeps of INTERVAL seconds."
  (let ((requested (round (* interval 1000000000))))
    (loop repeat count
          collect (let ((start (monotonic-ns)))
                    (sleep interval)
                    (- (monotonic-ns) start requested)))))

(defun calibration-outcome (overshoots)
  (if (<= (percentile overshoots 95/100) +wake-jitter-ceiling-ns+) :stable :unstable))

;;; Tokens on a screen or in a drawn grid

(defun token-indexes (nonce slots rows columns character-at)
  "Indexes of the complete tokens of this run in their own rows."
  (when (>= columns (+ +token-column+ +token-length+))
    (loop for row from 1 to (min slots (1- rows))
          for text = (let ((text (make-string +token-length+)))
                       (dotimes (offset +token-length+ text)
                         (let ((character (funcall character-at row (+ +token-column+ offset))))
                           (setf (char text offset)
                                 (if (characterp character) character #\Space)))))
          for index = (and (string= nonce text :end2 (length nonce))
                           (ignore-errors (parse-integer text :start (length nonce) :radix 36)))
          when (and index (= row (token-row index slots)) (string= text (token-text nonce index)))
            collect index)))

(defun screen-character (screen row column)
  (lispterm.engine:cell-character (lispterm.engine:cell-at screen row column)))

(defun screen-token-indexes (nonce slots screen)
  (token-indexes nonce slots (lispterm.engine:rows screen) (lispterm.engine:columns screen)
                 (lambda (row column) (screen-character screen row column))))

(defun grid-token-indexes (nonce slots grid)
  (token-indexes nonce slots (array-dimension grid 0) (array-dimension grid 1)
                 (lambda (row column) (ltd:cell-char (aref grid row column)))))

(defun screen-ready-p (nonce screen)
  (let ((text (ready-text nonce)))
    (and (>= (lispterm.engine:columns screen) (+ +token-column+ (length text)))
         (loop for offset below (length text)
               always (eql (char text offset)
                           (screen-character screen 0 (+ +token-column+ offset)))))))

;;; Recorder

(defstruct (recorder (:constructor make-recorder (nonce slots &key audit)))
  nonce slots
  audit ; NIL, or a function of (STORE PLIST) returning an alist of (token-row . pixel-hash)
  (events '())
  failure
  (active 0)
  closed-p
  (drained (sb-thread:make-waitqueue :name "latency reporters"))
  (lock (sb-thread:make-mutex :name "latency recorder")))

(defvar *active-recorder* nil)

(defun metadata (value)
  "VALUE when it is a number, keyword or boolean; other symbols by name only."
  (typecase value
    ((or integer keyword boolean) value)
    (symbol (intern (symbol-name value) :keyword))
    (t :redacted)))

(defun recorder-hook (recorder)
  "A synchronous hook. Admission precedes scanning/audit; retirement drains every
admitted reporter. A stale captured hook after retirement is not admitted. Audit
callbacks obey the production trace contract and must not reenter the recorder."
  (lambda (stage time &rest plist)
    (block report
      (when (eq *active-recorder* recorder)
        (setf (recorder-failure recorder) :recursive-recorder)
        (return-from report))
      (sb-thread:with-mutex ((recorder-lock recorder))
        (when (recorder-closed-p recorder)
          (return-from report))
        (incf (recorder-active recorder)))
      (let ((*active-recorder* recorder) (completed nil))
        (unwind-protect
             (handler-case
                 (let ((data '()))
                   (loop for (key value) on plist by #'cddr
                         do (case key
                              (:screen (push :tokens data)
                               (push (screen-token-indexes (recorder-nonce recorder)
                                                          (recorder-slots recorder) value) data))
                              (:grid (push :tokens data)
                               (push (grid-token-indexes (recorder-nonce recorder)
                                                        (recorder-slots recorder) value) data))
                              (:store
                               (when (recorder-audit recorder)
                                 (push :audit data)
                                 (push (funcall (recorder-audit recorder) value plist) data)))
                              (t (push key data) (push (metadata value) data))))
                   (sb-thread:with-mutex ((recorder-lock recorder))
                     (push (list* stage time (nreverse data)) (recorder-events recorder)))
                   (setf completed t))
               (error (condition)
                 (setf (recorder-failure recorder) condition
                       completed t)))
          (unless completed
            (setf (recorder-failure recorder) :unsupported-nonlocal-exit))
          (sb-thread:with-mutex ((recorder-lock recorder))
            (decf (recorder-active recorder))
            (sb-thread:condition-broadcast (recorder-drained recorder))))))))

(defun retire-recorder (recorder)
  "Close admission and wait for every admitted reporter, without copying events."
  (when (eq *active-recorder* recorder)
    (error 'measurement-error :detail "snapshot from inside its reporter"))
  (sb-thread:with-mutex ((recorder-lock recorder))
    (setf (recorder-closed-p recorder) t)
    (loop while (plusp (recorder-active recorder))
          do (sb-thread:condition-wait (recorder-drained recorder) (recorder-lock recorder)))))

(defun recorded-events (recorder)
  "Retire/drain RECORDER, then return events and a :SEALED recorder-local receipt.
A failed recorder yields no receipt. Use a fresh recorder to resume collection.
The cutoff is mutex admission, not the unsynchronized *TRACE* read. Only
CALL-WITH-RECORDER additionally checks the global trace boundary lifecycle."
  (retire-recorder recorder)
  (sb-thread:with-mutex ((recorder-lock recorder))
    (when (recorder-failure recorder)
      (error 'measurement-error :detail "recorder failed; evidence rejected"))
    (let ((events (reverse (recorder-events recorder))))
      (values events (list :state :sealed :closed-p t :active 0 :event-count (length events))))))

(defun call-with-recorder (recorder function)
  "Return events and a :COMPLETE receipt after draining and checking trace errors.
Do not concurrently replace the global hook. Admission precedes clock/argument
work; cleanup drains that whole boundary before retiring the recorder."
  (when lispterm.trace:*trace* (error 'measurement-error :detail "a trace hook is already set"))
  (sb-thread:with-mutex ((recorder-lock recorder))
    (when (recorder-closed-p recorder)
      (error 'measurement-error :detail "recorder is already retired")))
  (setf lispterm.trace:*trace-failure* nil)
  (unwind-protect
       (progn (setf lispterm.trace:*trace* (recorder-hook recorder))
              (funcall function))
    ;; Drain even when FUNCTION unwinds, but do not mask its original failure.
    ;; Clearing *TRACE* alone misses reporters paused before recorder admission.
    (handler-case
        (progn (lispterm.trace::retire-trace-boundary)
               (retire-recorder recorder))
      (error (condition) (setf (recorder-failure recorder) condition))))
  (multiple-value-bind (events snapshot) (recorded-events recorder)
    (when lispterm.trace:*trace-failure*
      (error 'measurement-error :detail "trace boundary failed; evidence rejected"))
    (values events (list :state :complete :snapshot snapshot :trace-failure nil))))
;;; Matcher

(defstruct sample
  index inject-before inject-after dispatch dequeue enqueue write
  probe-read probe-write probe-write-start reference pty-read parsed overwritten
  drawn generation copy commit commit-generation path buffer callback callback-at
  (intervening 0) (deferred 0) coalesced-p
  before-hash buffer-hash settled-hash
  status problem)

(defun row-hash (audit row) (cdr (assoc row audit)))

(defun attach-records (samples records what attach problem)
  "Give each record to its sample through ATTACH; report repeats and strangers."
  (let ((seen (make-hash-table)))
    (dolist (record records)
      (let ((index (first record)))
        (cond ((not (and (integerp index) (< -1 index (length samples))))
               (funcall problem "~A record for unknown sample ~S" what index))
              ((gethash index seen) (funcall problem "duplicate ~A record for sample ~D" what index))
              (t (setf (gethash index seen) t)
                 (funcall attach (aref samples index) record)))))))

(defun classify (sample input-path references-p)
  "Set SAMPLE's status: :matched, :unmatched, :superseded or :invalid."
  (flet ((settle (status &optional problem)
           (setf (sample-status sample) status (sample-problem sample) problem)))
    (cond ((null (sample-parsed sample)) (settle :unmatched "token was never parsed"))
          ((null (sample-commit sample))
           (if (sample-overwritten sample)
               (settle :superseded "token was overwritten before any commit carried it")
               (settle :unmatched "no commit carried the token")))
          (t
           (let ((missing (loop for (name value)
                                  in `(("key enqueue" ,(sample-enqueue sample))
                                       ("PTY write" ,(sample-write sample))
                                       ("probe read" ,(sample-probe-read sample))
                                       ("probe write" ,(sample-probe-write sample))
                                       ("probe write start" ,(sample-probe-write-start sample))
                                       ("PTY read" ,(sample-pty-read sample))
                                       ("draw" ,(sample-drawn sample))
                                       ,@(when references-p
                                           `(("reference arrival" ,(sample-reference sample))))
                                       ,@(when (eq input-path :display)
                                           `(("injection" ,(sample-inject-before sample))
                                             ("injection end" ,(sample-inject-after sample))
                                             ("key dispatch" ,(sample-dispatch sample))
                                             ("event dequeue" ,(sample-dequeue sample)))))
                                unless value collect name))
                 (chain (remove nil (list (sample-inject-before sample) (sample-dispatch sample)
                                          (sample-dequeue sample) (sample-enqueue sample)
                                          (sample-probe-read sample) (sample-pty-read sample)
                                          (sample-parsed sample) (sample-drawn sample)
                                          (sample-copy sample) (sample-commit sample)))))
             (cond (missing (settle :unmatched (format nil "missing ~{~A~^, ~}" missing)))
                   ((not (and (apply #'<= chain)
                              (<= (sample-enqueue sample) (sample-write sample))
                              ;; The post-write hook and subsequent read run on the same
                              ;; owner thread. Child read may precede that hook.
                              (<= (sample-write sample) (sample-pty-read sample))
                              (<= (sample-probe-read sample) (sample-probe-write-start sample)
                                  (sample-probe-write sample))
                              (<= (sample-probe-write-start sample) (sample-pty-read sample))
                              (<= (sample-probe-read sample) (sample-probe-write sample))
                              (or (null (sample-inject-after sample))
                                  (<= (sample-inject-before sample) (sample-inject-after sample)))
                              (or (null (sample-reference sample))
                                  (<= (sample-probe-read sample) (sample-reference sample)))))
                    (settle :invalid "stages are stale or out of order"))
                   (t (settle :matched))))))))

(defun match-run (events &key count slots probes injections references nonce audit-required
                            (input-path :display))
  "Chain every token of a run to the commit that first submitted it.
EVENTS are recorder events in report order; SLOTS is the run's token row count.
PROBES are (index byte read-ns write-end-ns write-start-ns nonce), INJECTIONS
(index before-ns after-ns), REFERENCES (index arrival-ns). NONCE must identify
this run. Without REFERENCES the signed diagnostic reference is write-end,
not output visibility. INPUT-PATH :DISPLAY requires injection and dispatch
records; :DIRECT does not. Return the sample vector and a report whose
:OUTCOME is :COMPLETE only when every sample matched and no problem was found."
  (let ((samples (make-array count)) (problems '()) (generation 0)
        (present (make-hash-table)) (copies (make-hash-table)) (previous-audit nil)
        (queue '()) (dispatches 0) (dequeues 0) (keys 0) (commits 0) (deferred 0)
        (last-read nil) (last-time nil))
    (dotimes (index count) (setf (aref samples index) (make-sample :index index)))
    (labels ((problem (control &rest arguments)
               (push (apply #'format nil control arguments) problems))
             (numbered (number what)
               (if (< number count)
                   (aref samples number)
                   (progn (problem "~A number ~D exceeds the ~D samples" what number count) nil)))
             (token-sample (index what)
               (if (< -1 index count)
                   (aref samples index)
                   (progn (problem "unknown token ~D ~A" index what) nil))))
      (unless (and (stringp nonce) (plusp (length nonce)))
        (problem "expected run nonce is missing"))
      (dolist (event events)
        (destructuring-bind (stage time &rest data) event
          (when (and last-time (< time last-time))
            (problem "~A at ~D precedes the previous event" stage time))
          (setf last-time time)
          (case stage
            (:key-dispatch
             (when (getf data :pressed)
               (let ((sample (numbered dispatches "key dispatch")))
                 (when sample (setf (sample-dispatch sample) time)))
               (incf dispatches)))
            (:event-dequeue
             (when (eq (getf data :kind) :key-event)
               (let ((sample (numbered dequeues "key dequeue")))
                 (when sample (setf (sample-dequeue sample) time)))
               (incf dequeues)))
            (:input-enqueue
             ;; Terminal replies share the queue; only key entries carry a sample.
             (let ((sample (unless (getf data :response-p)
                             (prog1 (numbered keys "key entry") (incf keys)))))
               (when sample (setf (sample-enqueue sample) time))
               (setf queue (nconc queue (list (cons (getf data :bytes) sample))))))
            (:pty-write
             (let ((accepted (getf data :accepted)))
               (loop while (plusp accepted)
                     do (let ((head (first queue)))
                          (unless head
                            (problem "PTY write of ~D bytes with an empty input queue" accepted)
                            (return))
                          (let ((used (min accepted (car head))))
                            (decf accepted used)
                            (decf (car head) used)
                            (when (zerop (car head))
                              (when (cdr head) (setf (sample-write (cdr head)) time))
                              (pop queue)))))))
            (:pty-read (setf last-read time))
            (:feed-done
             (let ((tokens (getf data :tokens)))
               (dolist (index tokens)
                 (let ((sample (token-sample index "parsed")))
                   (when (and sample (null (sample-parsed sample)))
                     (setf (sample-parsed sample) time (sample-pty-read sample) last-read))))
               (loop for sample across samples
                     when (and (sample-parsed sample) (null (sample-commit sample))
                               (null (sample-overwritten sample))
                               (not (member (sample-index sample) tokens)))
                       do (setf (sample-overwritten sample) time))))
            (:draw-start
             ;; Each draw is one content generation. A cursor-only draw keeps the
             ;; cells of the frame before it.
             (incf generation)
             (let ((tokens (if (eq (getf data :kind) :frame)
                               (getf data :tokens)
                               (gethash (1- generation) present))))
               (setf (gethash generation present) tokens)
               (dolist (index tokens)
                 (let ((sample (token-sample index "drawn")))
                   (when (and sample (null (sample-drawn sample)))
                     (unless (sample-parsed sample)
                       (problem "token ~D was drawn before it was parsed" index))
                     (setf (sample-drawn sample) time (sample-generation sample) generation))))))
            (:copy
             ;; The generation a buffer holds is the one current when its pixels were copied.
             (setf (gethash (getf data :buffer) copies)
                   (list generation time (getf data :audit))))
            (:commit
             (incf commits)
             (let* ((buffer (getf data :buffer)) (copy (gethash buffer copies)))
               (remhash buffer copies)
               (if (null copy)
                   (problem "commit ~D attaches buffer ~D without a recorded copy"
                            (getf data :generation) buffer)
                   (destructuring-bind (copied copied-at audit) copy
                     (let ((tokens (gethash copied present)) (submitted '()))
                       (flet ((row (sample audit)
                                (row-hash audit (token-row (sample-index sample) slots))))
                         (loop for sample across samples
                               for carried-p = (member (sample-index sample) tokens)
                               do (cond ((sample-commit sample)
                                         (when (and carried-p (null (sample-settled-hash sample)))
                                           (setf (sample-settled-hash sample) (row sample audit))))
                                        ((null (sample-parsed sample)))
                                        (carried-p (push sample submitted))
                                        (t (incf (sample-intervening sample)))))
                         (dolist (sample submitted)
                           (setf (sample-commit sample) time (sample-copy sample) copied-at
                                 (sample-commit-generation sample) (getf data :generation)
                                 (sample-path sample) (getf data :path)
                                 (sample-buffer sample) buffer
                                 (sample-callback sample) (getf data :callback)
                                 (sample-coalesced-p sample) (and (rest submitted) t)
                                 (sample-before-hash sample) (row sample previous-audit)
                                 (sample-buffer-hash sample) (row sample audit))))
                       (setf previous-audit audit))))))
            (:present-deferred
             (incf deferred)
             (loop for sample across samples
                   when (and (sample-drawn sample) (null (sample-commit sample)))
                     do (incf (sample-deferred sample))))
            (:frame-callback
             (loop for sample across samples
                   when (and (eql (sample-callback sample) (getf data :callback))
                             (null (sample-callback-at sample)))
                     do (setf (sample-callback-at sample) time))))))
      (attach-records samples probes "probe"
                      (lambda (sample record)
                         (if (not (= 6 (length record)))
                             (problem "probe record lacks v2 write bracket/run identity")
                             (destructuring-bind (index byte read written started identity) record
                               (unless (and nonce (equal nonce identity))
                                 (problem "probe run identity does not match expected nonce"))
                               (unless (eql byte (key-byte index))
                                 (problem "probe key identity ~D does not match sample ~D" byte index))
                               (setf (sample-probe-read sample) read
                                     (sample-probe-write sample) written
                                     (sample-probe-write-start sample) started))))
                      #'problem)
      (attach-records samples injections "injection"
                      (lambda (sample record)
                        (setf (sample-inject-before sample) (second record)
                              (sample-inject-after sample) (third record)))
                      #'problem)
      (attach-records samples references "reference"
                      (lambda (sample record) (setf (sample-reference sample) (second record)))
                      #'problem)
      (loop for sample across samples
            do (unless references
                 (setf (sample-reference sample) (sample-probe-write sample)))
               (classify sample input-path (and references t)))
      ;; The raw probe reads and writes serially on one thread. Across sample
      ;; identities, one completed answer must precede the next input read.
      (loop for index from 1 below count
            for previous = (aref samples (1- index))
            for current = (aref samples index)
            when (and (sample-probe-write previous) (sample-probe-read current)
                      (> (sample-probe-write previous) (sample-probe-read current)))
              do (problem "probe answer ~D completes after next input read" (1- index)))
      (when audit-required
        (let ((disagreements (audit-agreement samples)))
          (when disagreements
            (problem "required pixel audit missing or disagrees for samples ~S" disagreements))))
      (flet ((status-count (status) (count status samples :key #'sample-status)))
        (values samples
                (list :outcome (if (and (null problems) (= count (status-count :matched)))
                                   :complete :incomplete)
                      :problems (reverse problems)
                      :matched (status-count :matched) :unmatched (status-count :unmatched)
                      :superseded (status-count :superseded) :invalid (status-count :invalid)
                      :coalesced (count-if #'sample-coalesced-p samples)
                      :commits commits :deferred deferred))))))

(defun sample-intervals (sample)
  "Intervals of a matched SAMPLE in nanoseconds. Those starting at injection are
(lower upper) ranges, because the key left the injector between its two timestamps."
  (flet ((span (from to) (and from to (- to from))))
    (list :input-to-write (list (span (sample-inject-after sample) (sample-write sample))
                                (span (sample-inject-before sample) (sample-write sample)))
          :final-chunk-to-commit (span (sample-pty-read sample) (sample-commit sample))
          :parse-to-commit (span (sample-parsed sample) (sample-commit sample))
          :end-to-end (list (span (sample-inject-after sample) (sample-commit sample))
                            (span (sample-inject-before sample) (sample-commit sample)))
          :paired (span (sample-reference sample) (sample-commit sample))
          :probe-visibility-to-commit
          (list (max 0 (span (sample-probe-write sample) (sample-commit sample)))
                (span (sample-probe-write-start sample) (sample-commit sample)))
          :probe-write-overlaps-commit (> (sample-probe-write sample) (sample-commit sample))
          :commit-to-callback (span (sample-commit sample) (sample-callback-at sample)))))

(defun run-summary (samples &key (warm-up 0))
  "Summaries per interval over the matched samples after WARM-UP."
  (let ((intervals (loop for sample across samples
                         when (and (>= (sample-index sample) warm-up)
                                   (eq (sample-status sample) :matched))
                           collect (sample-intervals sample))))
    (loop for name in '(:final-chunk-to-commit :parse-to-commit :paired :commit-to-callback)
          for values = (remove nil (mapcar (lambda (plist) (getf plist name)) intervals))
          when values append (list name (summarize values)))))

(defun audit-agreement (samples)
  "Return legacy rejection indexes and a completeness/pixel-disagreement report.
The first value includes missing evidence for compatibility with positive callers.
The second is (:MATCHED N :COMPLETE N :MISSING ((INDEX FIELD ...) ...)
:DISAGREEMENTS (INDEX ...)); FIELD is :BEFORE, :COMMITTED or :SETTLED. Only complete
triples enter DISAGREEMENTS. Both positive and skipped-copy admission must require
COMPLETE=MATCHED=expected count and no MISSING, independently of disagreements."
  (let ((rejected '()) (missing '()) (disagreements '()) (matched 0) (complete 0))
    (loop for sample across samples
          when (eq (sample-status sample) :matched)
            do (incf matched)
               (let* ((index (sample-index sample))
                      (before (sample-before-hash sample))
                      (committed (sample-buffer-hash sample))
                      (settled (sample-settled-hash sample))
                      (absent (append (unless before (list :before))
                                      (unless committed (list :committed))
                                      (unless settled (list :settled)))))
                 (cond (absent
                        (push index rejected)
                        (push (cons index absent) missing))
                       (t
                        (incf complete)
                        (unless (and (eql committed settled) (not (eql committed before)))
                          (push index rejected)
                          (push index disagreements))))))
    (values (nreverse rejected)
            (list :matched matched :complete complete :missing (nreverse missing)
                  :disagreements (nreverse disagreements)))))

;;; Pixel audit of the shared-memory path. Audit runs are excluded from timing.

(defun pixel-rectangle-hash (pointer stride x y width height)
  "FNV-1a over the 32-bit pixels of a rectangle in a buffer STRIDE pixels wide."
  (let ((hash 2166136261))
    (loop for row from y below (+ y height)
          do (loop for column from x below (+ x width)
                   do (setf hash (logand #xffffffff
                                         (* 16777619
                                            (logxor hash (cffi:mem-aref pointer :uint32
                                                                        (+ column (* row stride)))))))))
    hash))

(defun shm-audit (slots cell-width cell-height)
  "An audit function for MAKE-RECORDER. At a :COPY it hashes each token row's
pixel rectangle in the shared-memory buffer about to be attached, and returns
(row . hash) pairs. Other buffer kinds give NIL, which never agrees."
  (lambda (store plist)
    (when (typep store 'lispterm.display.wayland::shm-buffer)
      (let* ((scale (getf plist :scale)) (stride (getf plist :width))
             (x (* +token-column+ cell-width scale))
             (width (* +token-length+ cell-width scale))
             (height (* cell-height scale)))
        (loop for row from 1 to slots
              for y = (* row height)
              when (and (<= (+ x width) stride) (<= (+ y height) (getf plist :height)))
                collect (cons row (pixel-rectangle-hash
                                   (lispterm.display.wayland::shm-buffer-pointer store)
                                   stride x y width height)))))))
