;;;; Keyboard native-evidence contract for the separately owned adapter.
;;;; Invocation, once per run, cwd = repository root:
;;;;   bash --noprofile --norc tests/vttest-keyboard-native.sh BACKEND RUN MANIFEST
;;;; BACKEND: string x11 or wayland. RUN: absolute run dir, no trailing slash.
;;;; MANIFEST: RUN/MANIFEST.sexp (original octets). Stdin null; output to
;;;; RUN/native.stdout, RUN/native.stderr. Exit 0 complete, 3 incomplete, else fail.
;;;; Recorder originals: unmodified vttest-gate.lisp artifacts for exact labels
;;;; keyboard-cursor,keyboard-keypad in RUN/recorder/: LABEL.transcript.raw
;;;; LABEL.input.raw LABEL.events.sexp LABEL.result.sexp LABEL.exit, NAME.png.
;;;; NAME is NNN-LABEL-CAPTURE (NNN decimal); CAPTURE is keysP-KEY, KEY the
;;;; lowercased keysym (up down left right kp_1 kp_2 kp_enter), or pause01.
;;;; Receipt RUN/KEYBOARD-RUN.sexp, one form, written once after post-close cleanup:
;;;;  (:kind :vttest-keyboard-native :version 1 :backend "x11"|"wayland"
;;;;   :manifest-sha256 HEX64 :status :pass|:fail|:incomplete
;;;;   :checks ((:scenario LABEL :capture CAPTURE :name NAME
;;;;             :pixels S :timing S) ...)        ; S is :pass, :fail or :unverified
;;;;   [:cleanup (:phase :post-close :state :stopped|:failed|:unknown|:incomplete
;;;;              :receipt RUN-RELATIVE-PATH :receipt-sha256 HEX64
;;;;              :owned ((:role STRING :pid N :start-token STRING
;;;;                       :state :stopped|:failed) ...)
;;;;              :wayland-holder :stopped|:failed (wayland), :not-applicable (x11))]
;;;;   [:offline-check (:exit N :log PATH :log-sha256 HEX64)])
;;;; Omit unverified checks. Missing cleanup is non-fresh; offline logs are not PASS.
(in-package #:vttest-keyboard-driver)

(defparameter +receipt+ "KEYBOARD-RUN.sexp")
(defparameter +recorder+ "recorder/")
(defparameter +statuses+ '(:pass :fail :unverified))
(defun need (test reason) (vttest-area::need test reason))
(defun artifact (dir name suffix) (concatenate 'string dir name suffix))
(defun hex-p (x) (vttest-area::hash-p x))
(defun text-p (x) (vttest-area::bounded-string-p x))

(defun run-file (root path)
  "Hash of an existing RUN-relative file named by the receipt."
  (need (and (text-p path) (char/= (char path 0) #\/)) :keyboard-receipt-path)
  (vttest-area::safe-components path)
  (digest (concatenate 'string root path)))

(defun validate-cleanup (c backend root)
  (vttest-area::plist-keys c '(:phase :state :receipt :receipt-sha256 :owned :wayland-holder))
  (need (and (eq (getf c :phase) :post-close) (member (getf c :state) '(:stopped :failed :unknown :incomplete))
             (hex-p (getf c :receipt-sha256))
             (member (getf c :wayland-holder)
                     (if (equal backend "wayland") '(:stopped :failed :unknown :incomplete) '(:not-applicable))))
        :keyboard-cleanup)
  (need (equal (getf c :receipt) "recorder/cleanup-receipt.sexp") :keyboard-cleanup-receipt)
  ;; Absence is reconciled by the independent consumer, not a digest failure.
  ;; No file linkage can be established without an actual run directory.
  (when (and root (present-p (concatenate 'string root (getf c :receipt))))
    (need (equal (run-file root (getf c :receipt)) (getf c :receipt-sha256))
          :keyboard-cleanup-digest))
  (vttest-area::proper (getf c :owned) 64)
  ;; Empty ownership evidence cannot establish cleanup.
  (dolist (o (getf c :owned))
    (vttest-area::plist-keys o '(:role :pid :start-token :state))
    (need (and (text-p (getf o :role)) (typep (getf o :pid) '(integer 1))
               (text-p (getf o :start-token)) (member (getf o :state) '(:stopped :failed :unknown :incomplete)))
          :keyboard-cleanup-owned)))

(defun validate-receipt (record backend root)
  "Structural receipt validation. Signals GATE-PROBLEM on wrong evidence."
  (vttest-area::plist-keys record
    '(:kind :version :backend :manifest-sha256 :status :checks :cleanup :offline-check)
    '(:kind :version :backend :manifest-sha256 :status :checks))
  (need (and (eq (getf record :kind) :vttest-keyboard-native) (eql (getf record :version) 1)
             (equal (getf record :backend) backend) (hex-p (getf record :manifest-sha256))
             (member (getf record :status) '(:pass :fail :incomplete)))
        :keyboard-receipt)
  (vttest-area::proper (getf record :checks) 64)
  (let ((seen nil))
    (dolist (k (getf record :checks))
      (vttest-area::plist-keys k '(:scenario :capture :name :pixels :timing))
      (need (and (case-of (getf k :scenario))
                 (member (getf k :capture) (capture-ids (getf k :scenario)) :test #'equal)
                 (vttest-area::identifier-p (getf k :name))
                 (member (getf k :pixels) +statuses+) (member (getf k :timing) +statuses+))
            :keyboard-receipt-check)
      (let ((id (list (getf k :scenario) (getf k :capture))))
        (need (not (member id seen :test #'equal)) :keyboard-receipt-duplicate-check)
        (push id seen))))
  (when (getf record :cleanup) (validate-cleanup (getf record :cleanup) backend root))
  (let ((o (getf record :offline-check)))
    (when o
      (vttest-area::plist-keys o '(:exit :log :log-sha256))
      (need (and (typep (getf o :exit) '(integer 0 255))
                 (hex-p (getf o :log-sha256))
                 (text-p (getf o :log)) (char/= (char (getf o :log) 0) #\/))
            :keyboard-offline-check)
      (vttest-area::safe-components (getf o :log))
      (when root
        (need (equal (run-file root (getf o :log)) (getf o :log-sha256))
              :keyboard-offline-check))))
  record)

(defun cleanup-status (record)
  ;; Failure-only fallback when no run directory is available to the consumer.
  (let ((c (getf record :cleanup)))
    (if (or (eq (getf c :state) :failed)
            (some (lambda (o) (eq (getf o :state) :failed)) (getf c :owned))
            (eq (getf c :wayland-holder) :failed))
        :fail :unverified)))

(defun prefix-p (actual expected)
  (and (<= (length actual) (length expected))
       (equal actual (subseq expected 0 (length actual)))))

(load (merge-pathnames "vttest-keyboard-prefix.lisp" *load-truename*))
