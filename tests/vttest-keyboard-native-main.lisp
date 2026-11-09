;;;; Closed native adapter. Timing and pixel evidence come from native-evidence.
(in-package #:vttest-keyboard-driver)
(unless (fboundp 'native-evidence)
  (load (merge-pathnames "vttest-keyboard-native-evidence.lisp" *load-truename*)))

(defun native-arguments (backend run manifest-path)
  "Validate before any request, stream or child is created."
  (need (member backend '("x11" "wayland") :test #'equal) :keyboard-native-backend)
  (need (and (stringp run) (> (length run) 1) (char= (char run 0) #\/)
             (char/= (char run (1- (length run))) #\/)) :keyboard-native-run)
  (let* ((root (concatenate 'string run "/")) (st (sb-posix:lstat run)))
    (need (and (sb-posix:s-isdir (sb-posix:stat-mode st))
               (= (sb-posix:stat-uid st) (sb-posix:getuid))
               (zerop (logand #o022 (sb-posix:stat-mode st)))
               (equal root (namestring (truename root)))
               (equal manifest-path (native-path root "MANIFEST.sexp"))
               (present-p manifest-path)
               (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:lstat manifest-path))))
          :keyboard-native-paths)
    root))
(defun native-linkage (root request-bytes)
  "Missing links are unfinished; observed wrong links are contradictions."
  (let ((copy (native-path root "recorder/area-request.sexp"))
        (receipt (native-path root "launch-receipt.txt")) (missing nil))
    (if (present-p copy)
        (need (equalp request-bytes (vttest-cursor-driver::bytes copy)) :keyboard-request-copy)
        (push :missing-request-copy missing))
    (if (present-p receipt)
        (need (equal (native-text receipt) (format nil "~arecorder~%" root))
              :keyboard-launch-receipt)
        (push :missing-launch-receipt missing))
    missing))
(defun native-main (backend run manifest-path)
  "Internal CLI entry. Exit 0 only for :PASS: every check's timing and pixels, both
aggregates, cleanup and linkage pass with no error. FAIL exits 1, incomplete 3."
  (let ((root nil) (manifest nil) (hash nil))
    ;; All argument/manifest failures are rejected before launch with exit 2.
    (handler-case
        (progn (setf root (native-arguments backend run manifest-path))
               (multiple-value-setq (manifest hash) (vttest-area:read-data manifest-path))
               (validate manifest)
               (need (equal backend (getf manifest :backend)) :keyboard-manifest-backend))
      (error (e) (format *error-output* "keyboard-native: ~a~%" e)
        (return-from native-main 2)))
    (let ((actual-gate-exit nil) (expected nil) (errors nil)
          (request (native-path root "launch-request.sexp"))
          (original nil) (request-hash nil)
          (cleanup nil) (cleanup-state :unverified)
          (checks nil) (cases nil) (failed nil) (transport nil) (font-proof nil)
          (scratch nil) (timing :unverified) (pixels :unverified) (status :incomplete))
      (handler-case
          (progn
            (native-put request
              (list :kind :vttest-keyboard-launch :version 1 :backend backend :nonce root
                    :manifest-sha256 hash :exact-labels '("keyboard-cursor" "keyboard-keypad")))
            (setf original (vttest-cursor-driver::bytes request)
                  request-hash (digest request))
            (multiple-value-setq (expected actual-gate-exit errors transport)
              (launch-keyboard-gate backend root request))
            ;; Numeric failure classification is separate from the actual record.
            (when (or (and actual-gate-exit (not (eql (getf actual-gate-exit :exit-code) 0)))
                      (some #'stringp errors))
              (setf failed t))
            (setf errors (append errors (native-linkage root original))))
        (vttest-area:gate-problem (e)
          (unless (= 3 (vttest-area:problem-code e)) (setf failed t))
          (push (princ-to-string e) errors))
        (error (e) (setf failed t) (push (princ-to-string e) errors)))
      ;; One lazy font closure per run; it verifies only when a case first calls it.
      (handler-case
          (if (fboundp 'make-font-proof)
              (when transport (setf font-proof (make-font-proof transport root)))
              (push :missing-font-consumer errors))
        (error (e) (push (princ-to-string e) errors)))
      ;; Owned pixel scratch: without it no pixel work runs, never a default directory.
      (handler-case
          (let ((dir (native-path root "pixel-scratch/")))
            (sb-posix:mkdir dir #o700)
            (setf scratch dir))
        (error (e) (push (list :pixel-scratch (princ-to-string e)) errors)))
      ;; Independent guards preserve both linkage and original-request diagnostics.
      (when original
        (handler-case
            (let ((after (vttest-cursor-driver::bytes request)) (after-hash (digest request)))
              (need (and (equalp original after) (equal request-hash after-hash))
                    :keyboard-request-changed))
          (error (e) (setf failed t) (push (princ-to-string e) errors))))
      ;; Manifest/source-pin validation is independent of both request checks.
      (handler-case
          (multiple-value-bind (after after-hash) (vttest-area:read-data manifest-path)
            (need (and (equal hash after-hash) (equal manifest after)) :keyboard-manifest-changed)
            (validate after))
        (error (e) (setf failed t) (push (princ-to-string e) errors)))
      (handler-case
          (if (fboundp 'consume-gate-cleanup)
              (multiple-value-bind (state reasons projection)
                  (consume-gate-cleanup root backend expected actual-gate-exit)
                (setf cleanup-state state cleanup projection errors (append errors reasons)))
              (push :missing-cleanup-consumer errors))
        (error (e) (setf failed t) (push (princ-to-string e) errors)))
      (multiple-value-bind (c bad e x) (guarded-native-evidence root font-proof scratch)
        (setf checks c failed (or failed bad) cases e errors (append errors x)))
      (setf timing (timing-aggregate checks)
            pixels (handler-case
                       (if (fboundp 'pixel-aggregate)
                           (pixel-aggregate (mapcar (lambda (k) (list :label (getf k :scenario)
                                                                      :id (getf k :capture)
                                                                      :pixels (getf k :pixels)))
                                                    checks)
                                            26)
                           :unverified)
                     (error (e) (push (princ-to-string e) errors) :unverified)))
      (when (or (eq cleanup-state :fail) (eq timing :fail) (eq pixels :fail)) (setf failed t))
      (setf status (cond (failed :fail)
                         ((and (null errors) (eq cleanup-state :pass)
                               (eq timing :pass) (eq pixels :pass))
                          :pass)
                         (t :incomplete)))
      (let ((record (list :kind :vttest-keyboard-native :version 1 :backend backend
                          :manifest-sha256 hash :status status
                          :checks checks)))
        (when cleanup (setf (getf record :cleanup) cleanup))
        (handler-case
            (progn
              (validate-receipt record backend root)
              (native-put (native-path root "NATIVE-OBSERVATIONS.sexp")
                          (list :gate-exit actual-gate-exit
                                :errors (subseq errors 0 (min 16 (length errors)))
                                :cleanup cleanup-state :cases cases
                                :pixels pixels :timing timing))
              (native-put (native-path root +receipt+) record))
          (error (e) (format *error-output* "keyboard-native receipt: ~a~%" e)
            (return-from native-main 1)))
        (cond (failed 1) ((eq status :pass) 0) (t 3))))))
