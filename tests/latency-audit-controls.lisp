;;;; Owned shared-memory audit and causal admission controls, no display/server.
(in-package #:lispterm.latency.controls)

(defun repair-match (events probes injections &key (nonce *nonce*) audit)
  (match-run events :count 2 :slots 4 :nonce nonce :probes probes
                   :injections injections :audit-required audit))

(defun repair-causality-controls ()
  (multiple-value-bind (events probes injections) (scripted-run 2)
    (flet ((probe-outcome (position value)
             (let ((changed (copy-tree probes)))
               (setf (nth position (first changed)) value)
               (getf (nth-value 1 (repair-match events changed injections)) :outcome))))
      (check :probe-write-start-after-read-impossible
             (eq :incomplete (probe-outcome 4 (key-time 0 71))))
      (check :probe-write-end-before-start-impossible
             (eq :incomplete (probe-outcome 3 (key-time 0 54))))
      (check :probe-stale-run-identity-rejected
             (eq :incomplete (probe-outcome 5 "XY")))
      ;; A child descheduled after its successful write can finish after commit.
      (check :probe-write-end-after-commit-is-legitimate
             (eq :complete (probe-outcome 3 (key-time 0 500))))
      (check :serial-probe-cannot-finish-after-next-read
             (eq :incomplete (probe-outcome 3 (key-time 1 51)))))
    (check :missing-expected-run-identity-rejected
           (eq :incomplete (getf (nth-value 1 (repair-match events probes injections :nonce nil))
                                :outcome)))
    (check :legacy-probe-record-without-bracket-rejected
           (eq :incomplete
               (getf (nth-value 1 (repair-match events (mapcar (lambda (p) (subseq p 0 4)) probes)
                                               injections)) :outcome)))
    ;; The PTY-write hook is post-syscall. Child consumption may precede it,
    ;; but the same owner cannot read its reply before that hook.
    (dolist (case '((65 :complete :child-read-before-write-hook-valid)
                    (75 :incomplete :owner-read-before-write-hook-impossible)))
      (destructuring-bind (offset expected name) case
        (let ((changed (copy-tree events)))
          (setf (second (find :pty-write changed :key #'first)) (key-time 0 offset))
          (setf changed (stable-sort changed #'< :key #'second))
          (check name (eq expected (getf (nth-value 1 (repair-match changed probes injections))
                                        :outcome))))))
    (let* ((recorder (make-recorder *nonce* 4))
           (reporter (recorder-hook recorder))
           (stamped (sb-thread:make-semaphore :count 0))
           (reported (sb-thread:make-semaphore :count 0)))
      ;; One reporter is descheduled between taking its stamp and recording.
      (let ((first (sb-thread:make-thread
                    (lambda ()
                      (let ((time (lispterm.trace:monotonic-ns)))
                        (sb-thread:signal-semaphore stamped)
                        (sb-thread:wait-on-semaphore reported)
                        (funcall reporter :control time))))))
        (sb-thread:wait-on-semaphore stamped)
        (funcall reporter :control (lispterm.trace:monotonic-ns))
        (sb-thread:signal-semaphore reported)
        (sb-thread:join-thread first))
      (let ((report (nth-value 1 (repair-match (append events (recorded-events recorder))
                                              probes injections))))
        (check :concurrent-timestamp-inversion-not-silently-sorted
               (and (= 2 (getf report :matched))
                    (eq :incomplete (getf report :outcome)) (getf report :problems)))))
    (let ((split (copy-tree events)))
      (push (list :pty-read (key-time 0 61) :bytes 1) split)
      (push (list :feed-done (key-time 0 62) :bytes 1 :tokens nil) split)
      (setf split (stable-sort split #'< :key #'second))
      (let ((sample (aref (repair-match split probes injections) 0)))
        (check :split-read-interval-is-final-chunk-only
               (= 60 (getf (sample-intervals sample) :final-chunk-to-commit)))))))

(defun real-shm-events (events scale skip-copy)
  "Replace every scripted hash with SHM-AUDIT of an owned shared mmap.
The source image uses distinct pixel values in token rectangles. MEMCPY is
performed on each copy unless SKIP-COPY names that copy index. No compositor."
  (let* ((width (* 32 scale)) (height (* 5 scale)) (size (* width height 4))
         (pointer (lispterm.display.wayland::%mmap (cffi:null-pointer) size 3 #x21 -1 0))
         (audit (shm-audit 4 1 1)) (copy-index 0) (tokens nil))
    (when (= (cffi:pointer-address pointer) #xffffffffffffffff)
      (error "owned shared mmap failed"))
    (unwind-protect
         (cffi:with-foreign-object (source :uint32 (* width height))
           (dotimes (i (* width height))
             (setf (cffi:mem-aref pointer :uint32 i) 0))
           (let ((store (lispterm.display.wayland::make-shm-buffer
                         :pointer pointer :width width :height height :size size)))
             (mapcar
              (lambda (event)
                (case (first event)
                  (:draw-start (setf tokens (getf (cddr event) :tokens)) event)
                  (:copy
                   (dotimes (i (* width height))
                     (setf (cffi:mem-aref source :uint32 i) 0))
                   (dolist (token tokens)
                     (loop for y from (* (token-row token 4) scale)
                           below (* (1+ (token-row token 4)) scale)
                           do (loop for x from (* +token-column+ scale)
                                    below (* (+ +token-column+ +token-length+) scale)
                                    do (setf (cffi:mem-aref source :uint32 (+ x (* y width)))
                                             (+ #xff010000 token)))))
                   (unless (eql skip-copy copy-index)
                     (cffi:foreign-funcall "memcpy" :pointer pointer :pointer source
                                           :unsigned-long size :pointer))
                   (incf copy-index)
                   (let ((copy (copy-tree event)))
                     (setf (getf (cddr copy) :audit)
                           (funcall audit store (list :scale scale :width width :height height)))
                     copy))
                  (t event)))
              events)))
      (lispterm.display.wayland::%munmap pointer size))))

(defun real-shm-controls ()
  (multiple-value-bind (events probes injections) (scripted-run 2)
    (dolist (scale '(1 2))
      (check (list :real-shm-copy-audit-admitted scale)
             (eq :complete (getf (nth-value 1
                                  (repair-match (real-shm-events events scale nil)
                                                probes injections :audit t)) :outcome)))
      (check (list :real-shm-skipped-copy-audit-rejected scale)
             (eq :incomplete (getf (nth-value 1
                                    (repair-match (real-shm-events events scale 1)
                                                  probes injections :audit t)) :outcome))))
    (let ((no-audit (copy-tree events)))
      (dolist (event no-audit) (remf (cddr event) :audit))
      (check :required-audit-missing-rejected
             (eq :incomplete (getf (nth-value 1 (repair-match no-audit probes injections :audit t))
                                  :outcome))))))

(defun recorder-overhead-controls ()
  "Controlled hook-on microbenchmark, not an estimate of native end-to-end lag.
Compare the actual trace-site clock plus full recorder against a clock-only hook
and NIL, alternating modes in seven rounds. Includes scans and recorder mutex."
  (dolist (geometry '((24 80 4) (120 240 119)))
    (destructuring-bind (rows columns slots) geometry
      (let* ((screen (lispterm.engine:make-screen :rows rows :columns columns))
             (grid nil)
             (results (make-hash-table)) (count 500))
        ;; All slots occupied, including token parsing/list allocation cost.
        (dotimes (index slots) (feed screen (token-bytes *nonce* index slots)))
        (setf grid (lispterm::screen-grid screen))
        (dotimes (round 7)
          (dolist (mode (if (evenp round) '(:off :clock-only :full) '(:full :clock-only :off)))
            (let* ((recorder (make-recorder *nonce* slots))
                   (hook (case mode (:full (recorder-hook recorder))
                                    (:clock-only (lambda (&rest args) (declare (ignore args))))))
                   (start (lispterm.trace:monotonic-ns)) (bytes (sb-ext:get-bytes-consed)))
              (unwind-protect
                   (progn
                     (setf lispterm.trace:*trace* hook)
                     (dotimes (i count)
                       (lispterm.trace:trace-event :feed-done :bytes 20 :screen screen)
                       (lispterm.trace:trace-event :draw-start :kind :frame :grid grid)))
                (setf lispterm.trace:*trace* nil))
              (push (list :ns-per-cycle (round (- (lispterm.trace:monotonic-ns) start) count)
                          :bytes-per-cycle (round (- (sb-ext:get-bytes-consed) bytes) count))
                    (gethash mode results)))))
        (format t "~S~%"
                (list :recorder-overhead :geometry geometry :occupied-slots slots
                      :cycles-per-round count :rounds 7
                      :modes (loop for mode in '(:off :clock-only :full)
                                   collect (list mode
                                                 :ns (summarize (mapcar (lambda (x) (getf x :ns-per-cycle))
                                                                       (gethash mode results)))
                                                 :bytes (mapcar (lambda (x) (getf x :bytes-per-cycle))
                                                                (gethash mode results))))))
        (check (list :recorder-benchmark-produced-seven-rounds geometry)
               (= 7 (length (gethash :full results))))))))

(setf *groups* (append *groups* '(repair-causality-controls real-shm-controls
                                recorder-overhead-controls)))
