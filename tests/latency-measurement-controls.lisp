;;;; Controls of the latency measurement itself: each check that can pass is
;;;; paired with an input that must make it fail. No display endpoint is used.
(defpackage #:lispterm.latency.controls
  (:use #:cl #:lispterm.latency #:lispterm.latency.tokens)
  (:export #:run))
(in-package #:lispterm.latency.controls)

(defvar *checks* 0)
(defvar *failures* '())
(defparameter *nonce* "QZ")

(defun check (name value)
  (incf *checks*)
  (unless value (push name *failures*))
  (format t "~S~%" (list :check name :pass (and value t)))
  (finish-output))

(defun signals-measurement-error-p (function)
  (handler-case (progn (funcall function) nil) (measurement-error () t)))

;;; Clock

(defun clock-controls ()
  (let ((report (clock-report)))
    (format t "~S~%" (list :clock report))
    (check :clock-monotonic-usable (null (clock-problems report)))
    (check :clock-id-is-monotonic (= 1 sb-unix:clock-monotonic))
    (let ((start (lispterm.trace:monotonic-ns)))
      (sleep 0.01)
      (check :clock-measures-a-sleep
             (< 9000000 (- (lispterm.trace:monotonic-ns) start) 500000000))))
  (let ((tick 0))
    (check :clock-rejects-millisecond-steps
           (clock-problems (clock-report :clock (lambda () (incf tick 1000000))
                                         :resolution 1000000 :reads 100))))
  (let ((tick 1000000))
    (check :clock-rejects-backward-steps
           (clock-problems (clock-report :clock (lambda () (decf tick 10))
                                         :resolution 1 :reads 100)))))

;;; Statistics and calibration

(defun statistics-controls ()
  (let ((values (loop for value from 1 to 100 collect value)))
    (check :percentile-median (= 50 (percentile values 1/2)))
    (check :percentile-95 (= 95 (percentile values 95/100)))
    (check :percentile-maximum (= 100 (percentile values 1))))
  (check :percentile-of-one-sample (= 7 (percentile '(7) 95/100)))
  (check :outlier-moves-maximum-not-median
         (let ((summary (summarize '(10 10 10 10 10 10 10 10 10 9000))))
           (and (= 10 (getf summary :median)) (= 9000 (getf summary :maximum))
                (= 9000 (getf summary :p95)))))
  (check :empty-samples-rejected (signals-measurement-error-p (lambda () (percentile '() 1/2))))
  (check :non-integer-samples-rejected
         (signals-measurement-error-p (lambda () (percentile '(1 2.5 3) 1/2))))
  (check :signed-differences-kept
         (let ((summary (summarize '(-5 10 20))))
           (and (= 1 (getf summary :negative)) (= -5 (getf summary :minimum)))))
  (check :range-passes-below (eq :pass (range-verdict 1 3 4)))
  (check :range-fails-above (eq :fail (range-verdict 5 9 4)))
  (check :range-straddling-is-inconclusive (eq :inconclusive (range-verdict 3 5 4)))
  (check :calibration-ceiling-is-one-millisecond (= 1000000 +wake-jitter-ceiling-ns+))
  (check :calibration-stable
         (eq :stable (calibration-outcome (make-list 100 :initial-element 100000))))
  (check :calibration-inflated-jitter-is-unstable
         (eq :unstable (calibration-outcome (append (make-list 90 :initial-element 100000)
                                                    (make-list 10 :initial-element 3000000)))))
  (format t "~S~%" (list :wake-jitter (summarize (measure-wake-jitter :count 50)))))

;;; Trace hook

(defvar *evaluated* 0)

(defun traced-site ()
  (lispterm.trace:trace-event :control :value (incf *evaluated*) :list (list 1 2 3)))

(defun trace-controls ()
  (check :trace-is-off-by-default (null lispterm.trace:*trace*))
  (setf *evaluated* 0)
  (let ((before (sb-ext:get-bytes-consed)))
    (dotimes (i 100000) (traced-site))
    (check :trace-off-allocates-nothing (< (- (sb-ext:get-bytes-consed) before) 4096)))
  (check :trace-off-evaluates-no-argument (zerop *evaluated*))
  (let ((seen '()))
    (unwind-protect
         (progn (setf lispterm.trace:*trace* (lambda (&rest event) (push event seen)))
                (dotimes (i 3) (traced-site)))
      (setf lispterm.trace:*trace* nil))
    (check :trace-on-evaluates-arguments (= 3 *evaluated*))
    (check :trace-on-reports-stage-time-and-data
           (and (= 3 (length seen))
                (every (lambda (event)
                         (and (eq :control (first event)) (integerp (second event))
                              (equal '(1 2 3) (getf (cddr event) :list))))
                       seen)
                (apply #'>= (mapcar #'second seen)))))
  ;; A real site: queued input is reported by size, never by content.
  (let ((session (lispterm::make-session nil 24 80)) (seen '()))
    (lispterm::session-write session (ascii-octets "abc"))
    (unwind-protect
         (progn (setf lispterm.trace:*trace* (lambda (&rest event) (push event seen)))
                (lispterm::session-write session (ascii-octets "secret")))
      (setf lispterm.trace:*trace* nil))
    (check :input-site-silent-while-off-and-metadata-only-while-on
           (and (= 1 (length seen))
                (eq :input-enqueue (first (first seen)))
                (equal '(:bytes 6 :response-p nil) (cddr (first seen))))))
  (let* ((recorder (make-recorder *nonce* 4)) (hook (recorder-hook recorder)))
    (funcall hook :control 1 :text "secret" :octets (ascii-octets "secret") :count 6
                             :kind 'lispterm.display:key-event)
    (check :recorder-keeps-metadata-only
           (equal '((:control 1 :text :redacted :octets :redacted :count 6 :kind :key-event))
                  (recorded-events recorder)))))

;;; Tokens on a real engine screen

(defun feed (screen octets &key (start 0) end)
  (lispterm.engine:feed screen (subseq octets start end)))

(defun screen-controls ()
  (let* ((slots 4)
         (screen (lispterm.engine:make-screen :rows 24 :columns 80))
         (recorder (make-recorder *nonce* slots))
         (hook (recorder-hook recorder))
         (token (token-bytes *nonce* 2 slots)))
    (check :ready-marker-absent (not (screen-ready-p *nonce* screen)))
    (feed screen (ready-bytes *nonce*))
    (check :ready-marker-seen (screen-ready-p *nonce* screen))
    (check :ready-marker-of-another-run-ignored (not (screen-ready-p "XY" screen)))
    ;; Split inside the cursor-position sequence, then inside the token text.
    (feed screen token :end 3)
    (check :token-absent-after-split-escape (null (screen-token-indexes *nonce* slots screen)))
    (feed screen token :start 3 :end (- (length token) 8))
    (funcall hook :feed-done 1 :bytes 1 :screen screen)
    (check :token-absent-while-incomplete (null (screen-token-indexes *nonce* slots screen)))
    (feed screen token :start (- (length token) 8))
    (funcall hook :feed-done 2 :bytes 1 :screen screen)
    (check :token-seen-when-complete (equal '(2) (screen-token-indexes *nonce* slots screen)))
    (check :hook-reduces-screen-to-token-indexes
           (equal '((:feed-done 1 :bytes 1 :tokens nil) (:feed-done 2 :bytes 1 :tokens (2)))
                  (recorded-events recorder)))
    ;; Status line and other output carrying token-like text elsewhere.
    (feed screen (ascii-octets (format nil "~C[24;1H[probe] 0:sbcl* ~A ~C[1;1H" #\Esc
                                       (token-text *nonce* 9) #\Esc)))
    (feed screen (ascii-octets (format nil "~C[2;~DH~A~C[1;1H" #\Esc (1+ +token-column+)
                                       (token-text "XY" 0) #\Esc)))
    ;; Token 1 written in the row that belongs to token 0.
    (feed screen (ascii-octets (format nil "~C[5;~DH~A~C[1;1H" #\Esc (1+ +token-column+)
                                       (token-text *nonce* 1) #\Esc)))
    (check :unrelated-output-adds-no-token
           (equal '(2) (screen-token-indexes *nonce* slots screen)))
    (check :drawn-grid-carries-the-same-tokens
           (equal '(2) (grid-token-indexes *nonce* slots (lispterm::screen-grid screen))))
    ;; Slot reuse: token 6 takes the row of token 2.
    (feed screen (token-bytes *nonce* 6 slots))
    (check :slot-reuse-replaces-token (equal '(6) (screen-token-indexes *nonce* slots screen)))))

;;; Matcher on scripted event streams

(defun present-tokens (index slots)
  "Tokens visible after key INDEX: the latest one in each row."
  (loop for token from (max 0 (- index slots -1)) to index collect token))

(defun audit-of (tokens slots)
  "A stand-in pixel hash per token row: distinct per token, zero for an empty row."
  (loop for row from 1 to slots
        for token = (find row tokens :key (lambda (token) (token-row token slots)))
        collect (cons row (if token (+ 100 token) 0))))

(defun scripted-run (count &key (slots 4) stale deferred coalesced skip-copy response)
  "Events, probe records and injection records of COUNT idle keys one
millisecond apart. The keyword lists name the keys that get a variation:
STALE commits an older frame between parse and render, DEFERRED presents
late, COALESCED leaves the frame to the next key, SKIP-COPY commits a buffer
that was not updated, RESPONSE queues a terminal reply ahead of the key."
  (let ((events '()) (probes '()) (injections '()) (buffer 0) (generation 0))
    (labels ((emit (&rest event) (push event events))
             (commit (at tokens audit)
               (declare (ignorable tokens))
               (emit :copy (- at 10) :path :shm :buffer (incf buffer) :audit audit)
               (emit :commit at :path :shm :buffer buffer :callback (+ 5000 buffer)
                               :generation (incf generation))
               (emit :frame-callback (+ at 300) :path :shm :callback (+ 5000 buffer))))
      ;; The ready frame. A real run commits it before the first key, so the
      ;; first token has a copy before it to differ from.
      (emit :draw-start 500000 :kind :frame :tokens '())
      (commit 500030 '() (audit-of '() slots))
      (dotimes (index count)
        (let* ((at (* (1+ index) 1000000))
               (tokens (present-tokens index slots))
               (earlier (audit-of (present-tokens (1- index) slots) slots)))
          (push (list index at (+ at 5)) injections)
          (emit :key-dispatch (+ at 10) :event-time index :pressed t)
          (emit :key-dispatch (+ at 900) :event-time index :pressed nil)
          (emit :event-dequeue (+ at 20) :kind :key-event)
          (when (member index response)
            (emit :input-enqueue (+ at 25) :bytes 4 :response-p t)
            (emit :pty-write (+ at 40) :accepted 2 :offered 4)
            (emit :pty-write (+ at 42) :accepted 2 :offered 2))
          (emit :input-enqueue (+ at 30) :bytes 1 :response-p nil)
          (emit :pty-write (+ at 45) :accepted 1 :offered 1)
          (push (list index (key-byte index) (+ at 50) (+ at 60) (+ at 55) *nonce*) probes)
          (emit :pty-read (+ at 70) :bytes 20)
          (emit :feed-done (+ at 80) :bytes 20 :tokens tokens)
          (when (member index stale) (commit (+ at 94) nil earlier))
          (unless (member index coalesced)
            (emit :render-start (+ at 96) :dirty t)
            (emit :draw-start (+ at 100) :kind :frame :tokens tokens)
            (emit :draw-end (+ at 110))
            (when (member index deferred)
              (emit :present-deferred (+ at 115) :path :shm :reason :frame-pending))
            (commit (if (member index deferred) (+ at 600) (+ at 130)) tokens
                    (if (member index skip-copy) earlier (audit-of tokens slots)))
            (emit :present-return (+ at 140)))))
      ;; A closing blink frame shows the same cells again.
      (let ((at (* (+ count 2) 1000000)) (tokens (present-tokens (1- count) slots)))
        (emit :draw-start at :kind :frame :tokens tokens)
        (commit (+ at 30) tokens (audit-of tokens slots))))
    (values (stable-sort (nreverse events) #'< :key #'second)
            (nreverse probes) (nreverse injections))))

(defun match-scripted (count &rest options &key (slots 4) (expect count) probes-filter
                                             injections-filter &allow-other-keys)
  "Match a scripted run of COUNT keys against EXPECT samples."
  (remf options :expect) (remf options :probes-filter) (remf options :injections-filter)
  (multiple-value-bind (events probes injections) (apply #'scripted-run count options)
    (match-run events :count expect :slots slots :nonce *nonce*
                      :probes (if probes-filter (funcall probes-filter probes) probes)
                      :injections (if injections-filter
                                      (funcall injections-filter injections)
                                      injections))))

(defun key-time (index offset) (+ (* (1+ index) 1000000) offset))

(defun matcher-controls ()
  (multiple-value-bind (samples report) (match-scripted 6)
    (check :idle-run-complete
           (and (eq :complete (getf report :outcome)) (= 6 (getf report :matched))
                ;; The ready frame, six keys and the closing frame.
                (zerop (getf report :deferred)) (= 8 (getf report :commits))))
    (check :idle-key-matches-its-own-commit
           (loop for sample across samples
                 always (and (= (sample-commit sample) (key-time (sample-index sample) 130))
                             (= (sample-write sample) (key-time (sample-index sample) 45))
                             (zerop (sample-intervening sample))
                             (not (sample-coalesced-p sample)))))
    (check :paired-difference-is-per-sample
           (let ((paired (getf (run-summary samples) :paired)))
             (and (= 6 (getf paired :count)) (= 70 (getf paired :median))
                  (= 70 (getf paired :maximum)))))
    (check :warm-up-excluded (= 4 (getf (getf (run-summary samples :warm-up 2) :paired) :count)))
    (check :injection-interval-is-a-range
           (equal '(40 45) (getf (sample-intervals (aref samples 0)) :input-to-write)))
    (check :callback-is-reported-apart-from-commit
           (= 300 (getf (sample-intervals (aref samples 0)) :commit-to-callback)))
    (check :audit-agrees-with-generation-chain (null (audit-agreement samples))))
  (multiple-value-bind (samples report) (match-scripted 6 :skip-copy '(3))
    (check :skipped-copy-still-matches-by-generation (eq :complete (getf report :outcome)))
    (check :skipped-copy-fails-the-pixel-audit (equal '(3) (audit-agreement samples))))
  (multiple-value-bind (samples report) (match-scripted 6 :stale '(2))
    (check :earlier-commit-after-parse-is-not-the-match
           (and (eq :complete (getf report :outcome))
                (= (sample-commit (aref samples 2)) (key-time 2 130))
                (= 1 (sample-intervening (aref samples 2))))))
  (multiple-value-bind (samples report) (match-scripted 6 :deferred '(4))
    (check :deferred-present-matches-the-later-commit
           (and (eq :complete (getf report :outcome)) (= 1 (getf report :deferred))
                (= (sample-commit (aref samples 4)) (key-time 4 600))
                (= 1 (sample-deferred (aref samples 4))))))
  (multiple-value-bind (samples report) (match-scripted 6 :coalesced '(1))
    (check :coalesced-keys-share-one-commit
           (and (eq :complete (getf report :outcome)) (= 2 (getf report :coalesced))
                (= (sample-commit (aref samples 1)) (sample-commit (aref samples 2))
                   (key-time 2 130))
                (sample-coalesced-p (aref samples 1)))))
  (multiple-value-bind (samples report) (match-scripted 6 :slots 1 :coalesced '(2))
    (check :overwritten-token-is-superseded
           (and (eq :incomplete (getf report :outcome)) (= 1 (getf report :superseded))
                (eq :superseded (sample-status (aref samples 2))))))
  (multiple-value-bind (samples report) (match-scripted 6 :response '(0 3))
    (check :terminal-replies-do-not-shift-key-writes
           (and (eq :complete (getf report :outcome))
                (= (sample-write (aref samples 3)) (key-time 3 45)))))
  (multiple-value-bind (samples report) (match-scripted 5 :expect 6)
    (check :swallowed-key-leaves-the-run-incomplete
           (and (eq :incomplete (getf report :outcome))
                (eq :unmatched (sample-status (aref samples 5))))))
  (flet ((outcome (&rest options) (getf (nth-value 1 (apply #'match-scripted 6 options)) :outcome)))
    (check :missing-probe-record-rejected
           (eq :incomplete (outcome :probes-filter (lambda (probes) (remove 3 probes :key #'first)))))
    (check :missing-injection-record-rejected
           (eq :incomplete (outcome :injections-filter #'rest)))
    (check :duplicate-probe-record-rejected
           (eq :incomplete (outcome :probes-filter (lambda (probes) (cons (first probes) probes)))))
    (check :reordered-key-byte-rejected
           (eq :incomplete
               (outcome :probes-filter
                        (lambda (probes)
                         (cons (list 0 (key-byte 1) (key-time 0 50) (key-time 0 60)
                                     (key-time 0 55) *nonce*)
                                (rest probes)))))))
  (multiple-value-bind (samples report)
      (match-scripted 6 :probes-filter
                      (lambda (probes)
                        (substitute (list 3 (key-byte 3) (key-time 2 50) (key-time 2 60)
                                          (key-time 2 55) *nonce*)
                                    3 probes :key #'first)))
    (check :stale-probe-time-rejected
           (and (eq :incomplete (getf report :outcome))
                (eq :invalid (sample-status (aref samples 3))))))
  (multiple-value-bind (events probes injections) (scripted-run 6)
    (flet ((outcome (events)
             (getf (nth-value 1 (match-run events :count 6 :slots 4 :nonce *nonce* :probes probes
                                                  :injections injections))
                   :outcome)))
      (check :out-of-order-events-rejected (eq :incomplete (outcome (reverse events))))
      (check :commit-without-copy-rejected
             (eq :incomplete
                 (outcome (append events (list (list :commit 99000000 :path :shm :buffer 777
                                                                      :callback 1 :generation 99))))))
      (check :missing-commit-rejected
             (eq :incomplete
                 (outcome (remove-if (lambda (event)
                                       (and (member (first event) '(:copy :commit))
                                            (< (key-time 5 0) (second event))))
                                     events)))))))

(defun audit-completeness-controls ()
  "Missing readbacks are rejected evidence, never successful skip mismatches."
  (dolist (skipped '(nil t))
    (dotimes (index 6)
      (dolist (key '(:before :committed :settled))
        (multiple-value-bind (samples matching)
            (match-scripted 6 :skip-copy (when skipped '(0 1 2 3 4 5)))
          (declare (ignore matching))
          (let ((sample (aref samples index)))
            (ecase key
              (:before (setf (lispterm.latency::sample-before-hash sample) nil))
              (:committed (setf (lispterm.latency::sample-buffer-hash sample) nil))
              (:settled (setf (lispterm.latency::sample-settled-hash sample) nil))))
          (multiple-value-bind (rejected completeness) (audit-agreement samples)
            (check (list :missing-readback skipped index key)
                   (and (member index rejected)
                        (eql 6 (getf completeness :matched))
                        (eql 5 (getf completeness :complete))
                        (equal (list (cons index (list key))) (getf completeness :missing))
                        (equal (when skipped (remove index '(0 1 2 3 4 5)))
                               (getf completeness :disagreements))))))))))

(defparameter *groups*
  '(clock-controls statistics-controls trace-controls screen-controls matcher-controls
    audit-completeness-controls)
  "The control groups RUN calls, in order. None needs tmux or a display endpoint;
tests/latency-tmux-controls.lisp appends its own group when it is loaded.")

(defun run ()
  "Run every control group; true when all passed."
  (setf *checks* 0 *failures* '())
  (dolist (group *groups*)
    (handler-case (funcall group)
      (error (condition)
        (push group *failures*)
        (format t "~S~%" (list :group group :error (princ-to-string condition))))))
  (format t "~S~%" (list :latency-measurement-controls :checks *checks*
                         :failed (reverse *failures*)))
  (null *failures*))
