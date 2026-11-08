;;;; Source-only public character-set acceptance controls.
;;;; Load only after the later runner has admitted the complete exact closure.
;;;; No load-time tests, recorder MAIN, bootstrap, or application objects.
(defpackage #:vttest-charsets-acceptance-tests
  (:use #:cl)
  (:export #:run-tests #:run-case #:case-plan))
(in-package #:vttest-charsets-acceptance-tests)
;; The later reader runner must create the original recorder package first.
;; These are its actual special variables, not replacement PATH/SAVE functions.
(declaim (special vttest-charsets-recorder::*root* vttest-charsets-recorder::*art*))

(defparameter +font+
  "/nix/store/9qvd27vg12177prxcl778rdhxh4lk2mi-terminus-font-4.49.1/share/fonts/terminus/ter-u16n.otb")
(defparameter +font-sha+ "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
(defparameter +magick+
  "/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick")
(defparameter +magick-sha+ "054c52b583c303bca2aae9fadcfd9e09fbc946f83e6621b39f22650d28d3027e")
(defvar *cache* nil)
(defvar *cache-root* nil)
(defvar *unique-pngs* nil)
(defvar *steps* nil)
(defvar *refusals* nil)
(defvar *boundary* :public-main)
(defvar *fixture-root* nil)
(defvar *native-refusal-text* nil)

(defun condition-facts (condition)
  "Retain original structured fields, never match a rendered error report."
  (cond ((typep condition 'vttest-area:gate-problem)
         (list :gate (vttest-area:problem-code condition) (vttest-area:problem-reason condition)))
        ((typep condition 'vttest-screen-frame:frame-refusal)
         (list :frame (copy-tree (vttest-screen-frame:refusal-reasons condition))))
        ((typep condition 'vttest-charsets-oracle:invalid-input)
         (let ((value (vttest-charsets-oracle:invalid-input-value condition)))
           (list :oracle (vttest-charsets-oracle:invalid-input-field condition)
                 (if (or (null value) (stringp value) (integerp value) (keywordp value)) value :cell))))
        ((typep condition 'file-error)
         (list :file (namestring (file-error-pathname condition))))
        ((typep condition 'simple-error)
         (list :simple (simple-condition-format-control condition)
               (mapcar (lambda (x) (if (or (stringp x) (integerp x) (keywordp x)) x :other))
                       (simple-condition-format-arguments condition))))
        (t (list :unexpected (symbol-name (type-of condition))))))
(defun observe-error (condition)
  (push (list :boundary *boundary* :facts (condition-facts condition)) *refusals*))
(defun step-call (name function)
  "Diagnostics only. Do not catch, normalize, or replace the original refusal."
  (let ((*boundary* name))
    (push (list name :entered) *steps*)
    (multiple-value-prog1 (funcall function) (push (list name :returned) *steps*))))
(defun returned-p (name) (member (list name :returned) *steps* :test #'equal))
(defun entered-p (name) (member (list name :entered) *steps* :test #'equal))
(defun at-refusal-p (boundary facts)
  (member (list :boundary boundary :facts facts) *refusals* :test #'equal))
(defun original-need-facts (reason) (list :simple "~a" (list reason)))
(defun save-diagnostics (directory)
  (put (concatenate 'string directory "BOUNDARIES.sexp")
       (list :synthetic-fixture-root *fixture-root* :steps (reverse *steps*)
           :original-refusals (reverse *refusals*) :native-refusal-text *native-refusal-text*)))

(defun insist (test control)
  (unless test (error "Character-set public control failed: ~s" control)))
(defun read-octets (path) (vttest-charsets-pixels::octets path))
(defun write-octets (path bytes)
  (with-open-file (s path :direction :output :if-exists :error
                         :element-type '(unsigned-byte 8))
    (write-sequence bytes s)))
(defun copy-octets (from to) (write-octets to (read-octets from)))
(defun data (path) (vttest-charsets-native-contract:read-data path))
(defun put (path object) (vttest-charsets-native-contract:write-data path object))

(defun public-result-preflight (text)
  "Bound public aggregate syntax before READ, including readable #A strings."
  (let ((i 0) (n (length text)) (depth 0) (tokens 0))
    (labels ((spacep (c) (find c '(#\Space #\Tab #\Newline #\Return)))
             (string-end (start)
               (loop with j = (1+ start) do
                 (insist (< j n) :public-unterminated-string)
                 (let ((c (char text j)))
                   (incf j)
                   (cond ((char= c #\\)
                          (insist (< j n) :public-unterminated-escape) (incf j))
                         ((char= c #\") (return j))))))
             (atom-p (token)
               (or (member token '("NIL" "T") :test #'string-equal)
                   (and (> (length token) 1) (char= (char token 0) #\:)
                        (every (lambda (c) (or (alphanumericp c) (find c "-_.+")))
                               (subseq token 1)))
                   (let ((start (if (find (char token 0) "+-") 1 0)))
                     (and (< start (length token))
                          (every #'digit-char-p (subseq token start)))))))
      (loop while (< i n) for c = (char text i) do
        (insist (<= (incf tokens) 100000) :public-token-limit)
        (cond ((spacep c) (incf i))
              ((char= c #\;) (setf i (or (position #\Newline text :start i) n)))
              ((char= c #\") (setf i (string-end i)))
              ((char= c #\() (incf i) (insist (<= (incf depth) 64) :public-syntax-depth))
              ((char= c #\)) (incf i) (insist (>= (decf depth) 0) :public-parentheses))
              ((char= c #\#)
               ;; The genuine manifest preflight admits only #A((n) BASE-CHAR .
               ;; "...") with n <= 4096. Never invoke READ on unchecked dispatch.
               (let* ((quote (position #\" text :start i))
                      (end (and quote (string-end quote))))
                 (insist end :public-array-string)
                 (loop while (and (< end n) (spacep (char text end))) do (incf end))
                 (insist (and (< end n) (char= (char text end) #\))) :public-array-close)
                 (incf end)
                 (insist (<= (+ depth 2) 64) :public-syntax-depth)
                 (vttest-area::preflight (subseq text i end))
                 (setf i end)))
              (t
               (let ((start i))
                 (loop while (and (< i n)
                                  (not (or (spacep (char text i))
                                           (find (char text i) "();\"#")))) do (incf i))
                 (insist (and (< start i) (<= (- i start) 256)
                              (atom-p (subseq text start i))) :public-token)))))
      (insist (zerop depth) :public-parentheses))))

(defun public-result-tree (object)
  "Proper transport lists: at most 64 list levels and 65536 visited nodes.
CDR links count as nodes, not nesting. Reject repeated conses, including cycles."
  (let ((seen (make-hash-table :test #'eq)) (nodes 0))
    (labels ((node () (insist (<= (incf nodes) 65536) :public-node-limit))
             (walk (x depth)
               (cond
                 ((consp x)
                  (insist (< depth 64) :public-list-depth)
                  (loop while (consp x) do
                    (node)
                    (insist (not (gethash x seen)) :public-shared-or-cyclic-list)
                    (setf (gethash x seen) t)
                    (walk (car x) (1+ depth))
                    (setf x (cdr x)))
                  (insist (null x) :public-improper-list)
                  (node))
                 (t (node)
                    (insist (or (null x) (eq x t) (keywordp x) (integerp x)
                                (and (stringp x) (<= (length x) 65536)))
                            :public-transport-type)))))
      (walk object 0))
    object))

(defun public-result (path)
  "Acceptance-only aggregate reader; DATA retains the production receipt policy."
  ;; Reuse the genuine bounded, no-follow regular-file descriptor opener.
  (let* ((fd (vttest-area::open-input-fd path))
         (stream (sb-sys:make-fd-stream fd :input t :element-type '(unsigned-byte 8)
                                       :auto-close t))
         (buffer (make-array 1048577 :element-type '(unsigned-byte 8)))
         (n (unwind-protect (read-sequence buffer stream) (close stream))))
    (insist (<= n 1048576) :public-byte-limit)
    (let ((text (sb-ext:octets-to-string (subseq buffer 0 n) :external-format :utf-8)))
      (public-result-preflight text)
      (let ((*readtable* (copy-readtable nil)) (*read-eval* nil)
            (*read-base* 10) (*package* (find-package :cl)))
        (with-input-from-string (s text)
          (let* ((eof (gensym)) (object (read s nil eof)))
            (insist (not (eq object eof)) :public-empty)
            (insist (eq (read s nil eof) eof) :public-one-object)
            (public-result-tree object)))))))
(defun ref (root path) (vttest-charsets-native-contract:reference root path))
(defun mkdir (path)
  (sb-posix:mkdir path #o700)
  (namestring (truename path)))
(defun replace-fixture (path object)
  "Retain the pre-mutation original. Only caller-owned synthetic artifacts here."
  (copy-octets path (concatenate 'string path ".before-mutation"))
  (delete-file path)
  (put path object))
(defun mutate-data (root relative function)
  (let ((path (concatenate 'string root relative)))
    (replace-fixture path (funcall function (data path)))))
(defun call-with-environment (bindings function)
  (let ((old (mapcar (lambda (b) (cons (car b) (sb-ext:posix-getenv (car b)))) bindings)))
    (unwind-protect
         (progn
           (dolist (b bindings)
             (if (cdr b) (sb-posix:setenv (car b) (cdr b) 1) (sb-posix:unsetenv (car b))))
           (funcall function))
      (dolist (b old)
        (if (cdr b) (sb-posix:setenv (car b) (cdr b) 1) (sb-posix:unsetenv (car b)))))))

(defun status (backend phase frames)
  "SYNTHETIC settled host data, not evidence of a window or a running client."
  (list :id 1 :child-pid 1 :backend (intern (string-upcase backend) :keyword)
        :rows 24 :columns 80 :pty-rows 24 :pty-columns 80 :frames frames
        :bytes-received 100 :blink-phase phase :blink-visible nil :input-pending 0
        :error-count 0 :process-state :running :transport-state :open
        :geometry (list :phase :settled :request-id 1 :display-epoch 1 :epoch 1
                        :actual-width 640 :actual-height 384 :cell-width 8 :cell-height 16
                        :presentation 1 :target-presentation 1 :rows 24 :columns 80
                        :window-id 41 :frame-pending-p nil
                        :commit-generation frames :frame-generation frames)))
(defun native-receipt (backend)
  (list :version 3 :backend (intern (string-upcase backend) :keyword) :pause 1
        :window-id 41 :native-window-id 41 :surface-id 42 :width 640 :height 384
        :scale 1 :capture-count 1 :owned t :capture-exit 0 :png "pause-01-actual.png"
        :ack-output (if (equal backend "x11") -1 100)
        :ack-basis :recorder-request-binding :custody t))

(defun bank-cell-index (set bank)
  ;; First printable ! sample in the block, G0/SI or G1/SO respectively.
  (+ (* (+ 3 (* 4 set)) 80) 10 (* bank 38)))
(defun fixture-snapshot (control)
  (let ((snapshot (vttest-charsets-oracle:character-set-snapshot)))
    (when (member (first control) '(:missing-bank :unresolved-bank))
      (let* ((cells (vttest-charsets-oracle:snapshot-cells snapshot))
             (index (bank-cell-index (second control) (third control))))
        (setf (aref cells index)
              (if (eq :missing-bank (first control)) nil
                  (vttest-screen-oracle:make-cell :code #xffff)))
        (setf snapshot (vttest-charsets-oracle::%snapshot cells))))
    snapshot))
(defun png-key (phase control)
  (if (member (first control) '(:pixel :alpha :geometry :omitted-bank))
      (list phase control) (list phase :complete)))
(defun verify-cache-entry (key entry)
  "Hardlinks are mutable. Rehash every retained payload and encoder receipt."
  (insist (and (equal key (getf entry :key))
               (equal +font-sha+ (getf entry :font-sha256))) :cache-key-and-font)
  (insist (equal (getf entry :raw-sha256)
                 (vttest-charsets-native-contract::digest (getf entry :raw))) :cache-raw-bytes)
  (insist (equal (getf entry :png-sha256)
                 (vttest-charsets-native-contract::digest (getf entry :png))) :cache-png-bytes)
  (dolist (receipt (getf entry :encoder))
    (insist (equal receipt (ref *cache-root* (getf receipt :path))) :cache-encoder-receipt))
  (insist (= 4 (length (getf entry :encoder))) :actual-encoder-references)
  (insist (eql 0 (data (concatenate 'string *cache-root* (getf entry :encoder-exit)))) :actual-encoder-exit)
  (getf entry :png))
(defun cached-png (phase control)
  "Encode synthetic reference RGBA once per distinct image. No fake command receipts.
Every returned unique PNG requires later executor AND primary visual inspection."
  (let* ((key (png-key phase control)) (cached (gethash key *cache*)))
      (if cached (verify-cache-entry key cached)
        (let* ((font (read-octets +font+))
               (sha (vttest-charsets-pixels::sha256 font))
               (snapshot (vttest-charsets-oracle:character-set-snapshot))
               (rgba (progn
                       (insist (equal sha +font-sha+) :actual-font-vector)
                       (vttest-charsets-oracle:rasterize-snapshot
                        snapshot font :font-sha256 sha :blink-phase phase)))
               (id (hash-table-count *cache*))
               (base (format nil "synthetic-~2,'0d-" id))
               (raw (concatenate 'string *cache-root* base "input.rgba"))
               (png (concatenate 'string *cache-root* base "encoded.png")))
          (insist (equal +magick-sha+ (vttest-charsets-native-contract::digest +magick+)) :encoder-pin)
          (case (first control)
            (:pixel (setf (aref rgba 0) (logxor 1 (aref rgba 0))))
            (:alpha (setf (aref rgba 3) 0))
            (:omitted-bank
             ;; Deliberately erase all sample cells in this bank, not a comparator mask.
             (let ((row (+ 3 (* 4 (second control)))) (bank (third control)))
               (loop for byte from 32 to 126 do
                 (let ((y (* 16 (1- (+ row (floor byte 32)))))
                       (x (* 8 (1- (+ 10 (* bank 38) (mod byte 32))))))
                   (dotimes (dy 16)
                     (dotimes (dx 8)
                       (dotimes (channel 3)
                         (setf (aref rgba (+ (* 4 (+ (* (+ y dy) 640) x dx)) channel)) 0)))))))))
          (write-octets raw rgba)
          (let ((receipts
                  (vttest-charsets-pixels::command
                   *cache-root* (concatenate 'string base "encode")
                   (append (list +magick+ "-size" "640x384" "-depth" "8" (concatenate 'string "rgba:" raw))
                           (when (eq :geometry (first control)) '("-crop" "639x384+0+0" "+repage"))
                           (list (concatenate 'string "PNG32:" png))))))
            (insist (equal +magick-sha+ (vttest-charsets-native-contract::digest +magick+)) :encoder-post-pin)
            (let ((entry (list :key key :font-sha256 sha :raw raw
                               :raw-sha256 (vttest-charsets-pixels::sha256 rgba)
                               :png png :png-sha256 (vttest-charsets-native-contract::digest png)
                               :encoder receipts :encoder-exit (concatenate 'string base "encode.exit"))))
              (put (concatenate 'string *cache-root* base "CACHE.sexp") entry)
              (push png *unique-pngs*)
              (setf (gethash key *cache*) entry)
              (verify-cache-entry key entry)))))))

(defun recorder-record (backend comparison native)
  "Synthetic recorder inputs. Actual SAVE, strict import, and publisher remain original."
  (list :kind :vttest-charsets-run :version 2 :backend backend :status :pass
        :attempted 1 :reached 1 :captured 1 :frame-verified 1 :bytes :unverified
        :wire-accounting :unverified :reason "SYNTHETIC public plumbing fixture"
        :cleanup :graceful-exit
        :checks (list (list :pause 1 :pixels :pass :bytes :unverified :timing :pass
                            :frame :pass :native native :comparison comparison))
        :evidence '("events.sexp" "output.raw" "input.raw" "application.log" "launch.sexp" "terminal-status.sexp")))

(defun mutate-recorder (record control)
  (case (first control)
    (:unknown-original (setf record (append record '(:unknown t))))
    (:duplicate-original (setf record (append record '(:version 2))))
    (:wrong-recorder-backend (setf (getf record :backend) "other"))
    (:missing-check (setf (getf record :checks) nil))
    (:duplicate-check (setf (getf record :checks)
                           (append (getf record :checks) (copy-tree (getf record :checks)))))
    (:wrong-pause (setf (getf (first (getf record :checks)) :pause) 2))
    (:wrong-count (setf (getf record :captured) 0))
    (:incomplete-recorder (setf (getf record :frame-verified) 0))
    (:close-failure (setf (getf record :cleanup) '(:graceful-exit :close-error "synthetic"))))
  record)

(defun mutate-comparison (root control)
  (when (member (first control) '(:comparison-phase :comparison-facts :length))
    (if (eq :length (first control))
        (let ((path (concatenate 'string root "recorder/pause-01-actual.rgba")))
          (copy-octets path (concatenate 'string path ".before-mutation"))
          (delete-file path)
          (write-octets path (make-array 4 :element-type '(unsigned-byte 8) :initial-element 255))
          (mutate-data root "recorder/pause-01-comparison.sexp"
            (lambda (r) (setf (getf r :rgba) (ref root "recorder/pause-01-actual.rgba")) r)))
        (mutate-data root "recorder/pause-01-comparison.sexp"
          (lambda (r)
            (if (eq :comparison-phase (first control))
                (setf (getf r :phase) (not (getf r :phase)))
                (remf (getf r :comparison) :compared-pixels))
            r)))))

(defun native-fault-p (control)
  (member (first control) '(:missing-stage :duplicate-stage :incomplete-native :native-digest
                            :altered-original-bytes :late-import :manifest-after)))
(defun native-refusal-reason (control)
  (case (first control)
    ((:missing-stage :incomplete-native) "Ledger count mismatch")
    (:altered-original-bytes "Reference hash mismatch")
    (:late-import "Late import")
    (otherwise "Native contract refusal")))
(defun apply-native-fault (root backend manifest-path control)
  "Preserve the once-published valid native baseline, then change owned fixture bytes.
  The supplemental original validator never replaces the subsequent actual driver call."
  (when (native-fault-p control)
    (let ((path (concatenate 'string root "CHARSETS-RUN.sexp")))
      (step-call :valid-native
        (lambda () (vttest-charsets-native-contract:validate-native (data path) backend root)))
      (copy-octets path (concatenate 'string path ".valid-baseline"))
      (case (first control)
        ((:missing-stage :duplicate-stage :incomplete-native)
         (mutate-data root "native/stages.sexp"
           (lambda (ledger)
             (let ((entries (getf ledger :entries)))
               (setf (getf ledger :entries)
                     (case (first control)
                       (:missing-stage nil)
                       (:duplicate-stage (append entries (copy-tree entries)))
                       (:incomplete-native
                        (let ((entry (first entries)))
                          (setf (getf entry :completed) nil (getf entry :capture-exit) nil
                                (getf entry :exit-origin) nil (getf entry :receipt) nil)
                          entries))))) ledger))
         ;; Hash rebinding exposes the intended ledger defect, not a stale-reference refusal.
         (mutate-data root "CHARSETS-RUN.sexp"
           (lambda (record)
             (setf (getf (getf (getf record :stages) :native) :origin)
                   (ref root "native/stages.sexp")) record)))
        (:native-digest
         (mutate-data root "CHARSETS-RUN.sexp"
           (lambda (record)
             (setf (getf record :recorder-sha256) (make-string 64 :initial-element #\0)) record)))
        (:altered-original-bytes
         (let ((original (concatenate 'string root "recorder/RESULT.sexp")))
           (copy-octets original (concatenate 'string original ".before-mutation"))
           (with-open-file (s original :direction :output :if-exists :append) (terpri s))))
        (:late-import
         (mutate-data root "native/import-deadline.sexp"
           (lambda (clock) (setf (getf clock :validated-ns) (getf clock :deadline-ns)) clock)))
        (:manifest-after
         (copy-octets manifest-path (concatenate 'string manifest-path ".before-mutation"))
         (with-open-file (s manifest-path :direction :output :if-exists :append) (terpri s))))
      (push '(:native-fault :returned) *steps*)
      (put (concatenate 'string root "NATIVE-FAULT-BYTES.sexp")
           (list :control control :native (ref root "CHARSETS-RUN.sexp")
                 :ledger (ref root "native/stages.sexp")
                 :manifest (ref root "MANIFEST.sexp")
                 :deadline (ref root "native/import-deadline.sexp")))
      (handler-case
          (step-call :changed-native
            (lambda ()
              (handler-bind ((error #'observe-error))
                (vttest-charsets-native-contract:validate-native (data path) backend root))))
        (error (condition) (setf *native-refusal-text* (princ-to-string condition)))))))

(defun materialize-fixture (backend root manifest-path manifest-bytes phase control)
  "ONLY invoked at INVOKE-NATIVE. No native process, build, client, or capture runs.
The encoder/identify/full decoder DO run in a later authorized test invocation."
  ;; Assert the driver staged BEFORE allowing any fixture activity. This catches B1.
  (insist (equal manifest-path (concatenate 'string root "MANIFEST.sexp")) :run-local-manifest-path)
  (insist (equal (namestring (truename manifest-path)) manifest-path) :canonical-manifest)
  (insist (equalp manifest-bytes (read-octets manifest-path)) :same-octet-manifest)
  (setf *fixture-root* root)
  (push '(:routing :returned) *steps*)
  (mkdir (concatenate 'string root "recorder/"))
  (mkdir (concatenate 'string root "native/"))
  (put (concatenate 'string root "SYNTHETIC.sexp")
       (list :kind :synthetic-native-inputs :backend backend :control control :phase phase
             :actual-backend-run nil :actual-build nil :actual-client nil :actual-capture nil))
  (let* ((before (status backend phase 1)) (ready (status backend phase 2))
         (pre (copy-tree ready)) (final (copy-tree ready))
         (native (native-receipt backend)) (snapshot (vttest-charsets-oracle:character-set-snapshot))
         (ack (getf native :ack-output)) (comparison nil))
    (step-call :valid-bracket
      (lambda () (vttest-screen-frame:validate-frame before ready pre final native
                   :pause 1 :target 100 :blink-content nil :endpoint nil :ack-output ack :surface-id 42)))
    (step-call :valid-snapshot (lambda () (vttest-charsets-oracle:require-complete snapshot)))
    (setf snapshot (fixture-snapshot control))
    (case (first control)
      (:missing-phase (remf ready :blink-phase))
      (:nonboolean-phase (setf (getf ready :blink-phase) 1))
      (:unstable-phase (setf (getf final :blink-phase) (not phase)))
      (:profile (setf (getf (getf ready :geometry) :cell-width) 9))
      (:native-failure (setf (getf native :capture-exit) 1)))
    (loop for name in '("before" "ready" "pre" "final" "native")
          for object in (list before ready pre final native) do
            (put (concatenate 'string root "recorder/pause-01-" name ".sexp") object))
    (put (concatenate 'string root "recorder/pause-01-request.sexp")
         (list :pause 1 :status ready :target 100 :ack-output ack :surface-id 42))
    (put (concatenate 'string root "recorder/pause-01-request") '(:synthetic-request t))
    (put (concatenate 'string root "recorder/pause-01-capture.exit") (getf native :capture-exit))
    (when (eq :wrong-font-vector (first control))
      ;; Deliberately corrupt a private test vector, never the font/provider binding.
      (let ((font (read-octets +font+)))
        (insist (equal +font-sha+ (vttest-charsets-pixels::sha256 font)) :unmodified-font-vector-baseline)
        (setf (aref font 0) (logxor 1 (aref font 0)))
        ;; If admission wrongly succeeds, continue to the normal publication path.
        ;; The required nonzero public result must come from the original refusal.
        (put (concatenate 'string root "WRONG-FONT.sexp")
             (list :synthetic t :changed-byte 0 :sha256 (vttest-charsets-pixels::sha256 font)))
        (step-call :font-admission
          (lambda ()
            (vttest-charsets-oracle:rasterize-snapshot snapshot font
              :font-sha256 (vttest-charsets-pixels::sha256 font) :blink-phase phase)))))
    (step-call :encode
      (lambda ()
        (sb-posix:link (cached-png (if (eq :wrong-comparison-phase (first control)) (not phase) phase) control)
                       (concatenate 'string root "recorder/pause-01-actual.png"))))
    (setf comparison
          (step-call :comparison
            (lambda () (vttest-charsets-pixels:compare-capture
                        root 1 snapshot before ready pre final native 100 ack 42))))
    (let* ((vttest-charsets-recorder::*root* (pathname root))
           (vttest-charsets-recorder::*art* (merge-pathnames "recorder/" (pathname root)))
           (record (recorder-record backend comparison native))
           (path (concatenate 'string root "recorder/RESULT.sexp")))
      (vttest-charsets-recorder::save "events.sexp" '(:synthetic t :wire-proof :unverified))
      (vttest-charsets-recorder::save "launch.sexp" '(:synthetic t :launched nil))
      (vttest-charsets-recorder::save "terminal-status.sexp" final)
      (write-octets (concatenate 'string root "recorder/output.raw") #())
      (write-octets (concatenate 'string root "recorder/input.raw") #())
      (with-open-file (s (concatenate 'string root "recorder/application.log")
                         :direction :output :if-exists :error)
        (write-line "SYNTHETIC fixture. No application was launched." s))
      (step-call :writer (lambda () (vttest-charsets-recorder::save "RESULT.sexp" record)))
      (step-call :valid-recorder
        (lambda () (vttest-charsets-native-contract:validate-recorder (data path) backend root)))
      ;; First establish real valid original bytes, then apply the sole selected fault.
      (let ((changed (mutate-recorder (copy-tree record) control)))
        (mutate-comparison root control)
        (when (member (first control) '(:comparison-phase :comparison-facts :length))
          (setf (getf (first (getf changed :checks)) :comparison)
                (ref root "recorder/pause-01-comparison.sexp")))
        (unless (equal record changed) (replace-fixture path changed)))
      (case (first control)
        (:missing-original
         (copy-octets path (concatenate 'string path ".before-mutation")) (delete-file path))
        (:malformed-original (replace-fixture path :malformed))
        (:trailing-original
         (copy-octets path (concatenate 'string path ".before-mutation"))
         (with-open-file (s path :direction :output :if-exists :append) (write-string " NIL" s))))
      (push '(:recorder-fault :returned) *steps*)
      ;; Original importer, exact canonical original and byte origin, never an alternate importer.
      (step-call :import (lambda () (vttest-charsets-native-contract:import-recorder root backend))))
    (with-open-file (s (concatenate 'string root "native/stage-events.sexp")
                       :direction :output :if-exists :error)
      (dolist (row '((1 :request) (1 :invoked) (1 :completed 0)))
        (write row :stream s) (terpri s)))
    (with-open-file (s (concatenate 'string root "native/unowned-endpoints.txt")
                       :direction :output :if-exists :error))
    (put (concatenate 'string root "native/client.exit") (if (eq :client-failure (first control)) 1 0))
    ;; SYNTHETIC clock origins exercise validation, never claim a timed client existed.
    (put (concatenate 'string root "native/finalization-clock.sexp")
         (list :clock :clock-monotonic :limit-seconds 15 :start-ns 100 :end-ns 200
               :state :exited :pid 1 :token "synthetic" :run root :backend backend))
    (put (concatenate 'string root "native/import-deadline.sexp")
         (list :clock :clock-monotonic :deadline-ns 15000000100
               :validated-ns 300 :state :valid))
    (let ((env (list (cons "ROOT" root) (cons "BACKEND" backend)
                     (cons "FINALIZATION" (if (eq :finalization-failure (first control)) ":timeout" ":complete"))
                     (cons "CLIENT_EXIT" (if (eq :client-failure (first control)) "1" "0"))
                     (cons "STATUS" ":pass")
                     (cons "BUILD" (if (eq :build-failure (first control)) ":fail" ":pass"))
                     (cons "STARTED" "T")
                     (cons "CLEANUP" (if (eq :cleanup-failure (first control)) ":fail" ":pass"))
                     (cons "ATTEMPTED" "1") (cons "REACHED" "1") (cons "CAPTURED" "1")
                     (cons "REASON" "SYNTHETIC public plumbing fixture"))))
      (step-call :publication
        (lambda ()
          (call-with-environment
           (mapcar (lambda (b) (cons (concatenate 'string "LISPTERM_CHARSETS_" (car b)) (cdr b))) env)
           #'vttest-charsets-native-contract:publish-native))))
    (apply-native-fault root backend manifest-path control)
    0))

(defun manifest-control-p (control)
  (member (first control) '(:area :backend :driver :menu :source :archive :digest)))
(defun mutate-manifest (manifest control)
  (case (first control)
    (:area (setf (getf manifest :area) "screen"))
    (:backend (setf (getf manifest :backend) "other"))
    (:driver (setf (getf manifest :native-driver) "screen-x11-v1"))
    (:menu (setf (getf (first (getf manifest :scenarios)) :menu-path) '(4)))
    ((:source :archive :digest)
     (let* ((pins (getf manifest :pins))
            (pin (if (eq :archive (first control)) (first pins)
                     (find "tests/vttest-charsets-oracle.lisp" pins :test #'equal
                           :key (lambda (p) (getf p :path))))))
       (insist pin :manifest-pin-present)
       (if (eq :source (first control)) (setf (getf pin :path) "tests/other.lisp")
           (setf (getf pin :sha256) (make-string 64 :initial-element #\0))))))
  manifest)

(defun public-result-path (stdout)
  (with-input-from-string (s stdout)
    (loop for line = (read-line s nil) while line
          when (and (>= (length line) 7) (string= "RESULT " line :end2 7))
            return (subseq line 7))))
(defun require-prefix (&rest names)
  (dolist (name names) (insist (returned-p name) (list :successful-prefix name))))
(defun assert-boundary (result control)
  "A public failure alone is never a passing negative control."
  (let ((kind (first control)))
    (unless (manifest-control-p control) (require-prefix :routing :valid-bracket :valid-snapshot))
    (require-prefix :valid-manifest)
    (cond
      ((manifest-control-p control)
       (let ((reason (case kind
                       ((:area :backend) :manifest-selector)
                       (:menu :recorder-selector)
                       (:driver :driver-not-integrated)
                       (otherwise :charsets-manifest-mismatch))))
         (insist (and (at-refusal-p :changed-manifest (list :gate 2 reason))
                      (eql 2 (getf result :exit))
                      (eq reason (getf result :reason)) (not (returned-p :routing)))
                 :selected-manifest-refusal)))
      ((eq kind :positive) (require-prefix :routing :encode :comparison :writer :valid-recorder :import :publication))
      ((native-fault-p control)
       (require-prefix :routing :encode :comparison :writer :valid-recorder :import :publication :valid-native :native-fault)
       (let* ((expected (original-need-facts (native-refusal-reason control)))
              (binding (data (concatenate 'string *fixture-root* "NATIVE-FAULT-BYTES.sexp")))
              (baseline (data (concatenate 'string *fixture-root* "CHARSETS-RUN.sexp.valid-baseline"))))
         (insist (and (eq :pass (getf baseline :status))
                      (at-refusal-p :changed-native expected) (not (returned-p :changed-native))
                      *native-refusal-text*
                      (equal *native-refusal-text* (getf (getf result :driver-evidence) :receipt-error)))
                 :same-original-validator-and-driver-refusal)
         (loop for key in '(:native :ledger :manifest :deadline)
               for path in '("CHARSETS-RUN.sexp" "native/stages.sexp" "MANIFEST.sexp" "native/import-deadline.sexp") do
                 (insist (equal (getf binding key) (ref *fixture-root* path)) :driver-read-same-fault-bytes))
         (when (eq kind :late-import)
           (let ((before (data (concatenate 'string *fixture-root* "native/import-deadline.sexp.before-mutation")))
                 (after (data (concatenate 'string *fixture-root* "native/import-deadline.sexp")))
                 (imported (data (concatenate 'string *fixture-root* "native/recorder-import.sexp"))))
             (insist (and (eql 300 (getf before :validated-ns))
                          (eql 15000000100 (getf before :deadline-ns))
                          (eql 15000000100 (getf after :validated-ns))
                          (equal after (let ((copy (copy-tree before)))
                                         (setf (getf copy :validated-ns) 15000000100) copy))
                          (eq :pass (getf imported :status))
                          (equal (getf imported :origin) (ref *fixture-root* "recorder/RESULT.sexp")))
                     :actual-late-import-after-successful-strict-import)))
         (when (eq kind :manifest-after)
           (let ((binding (data (concatenate 'string *fixture-root* "SOURCE-BINDING.sexp"))))
             (insist (and (getf binding :post-run-error)
                          (equal (getf binding :post-run-error)
                                 (getf (getf result :driver-evidence) :receipt-read-error)))
                     :both-path-posthash-refusal)))))
      ((member kind '(:client-failure :finalization-failure))
       (require-prefix :routing :encode :comparison :writer :valid-recorder :import :publication)
       (insist (null *refusals*) :no-earlier-library-refusal))
      ((member kind '(:build-failure :cleanup-failure))
       (require-prefix :routing :encode :comparison :writer :valid-recorder :import)
       (insist (and (entered-p :publication) (not (returned-p :publication))
                    (at-refusal-p :publication (original-need-facts "Failure downgraded")))
               :original-failed-build-or-cleanup-cannot-publish-pass))
      ((eq kind :wrong-font-vector)
       (require-prefix :routing)
       (let ((fault (data (concatenate 'string *fixture-root* "WRONG-FONT.sexp"))))
         (insist (and (not (equal +font-sha+ (getf fault :sha256)))
                      (at-refusal-p :font-admission (list :oracle :font-sha256 (getf fault :sha256)))
                      (not (returned-p :font-admission))) :actual-font-admission-refusal)))
      ((member kind '(:missing-bank :unresolved-bank :missing-phase :nonboolean-phase :unstable-phase :profile :native-failure))
       (require-prefix :routing :encode)
       (insist (and (entered-p :comparison) (not (returned-p :comparison))) :comparison-refused)
       (insist
        (case kind
          (:missing-bank (at-refusal-p :comparison '(:oracle :cell nil)))
          (:unresolved-bank (or (at-refusal-p :comparison '(:oracle :glyph 0))
                                (at-refusal-p :comparison '(:oracle :glyph-record 65535))))
          (otherwise
           (let ((reason (case kind (:missing-phase '(:missing :blink-phase))
                              (:nonboolean-phase '(:boolean :blink-phase))
                              (:unstable-phase :ready-final) (:profile :approved-grid)
                              (:native-failure :capture-exit))))
             (some (lambda (event)
                     (let ((facts (getf event :facts)))
                       (and (eq :comparison (getf event :boundary)) (eq :frame (first facts))
                            (member reason (second facts) :test #'equal)))) *refusals*))))
        :original-typed-target-refusal))
      ((member kind '(:geometry :alpha :pixel :wrong-comparison-phase :omitted-bank))
       (require-prefix :routing :encode)
       (let ((facts (case kind
                      (:geometry '(:simple "PNG dimensions refusal" nil))
                      (:alpha '(:simple "Decoded full opaque RGBA refusal" nil))
                      (otherwise '(:simple "Exact character-set raster mismatch at pause ~d" (1))))))
         (insist (and (at-refusal-p :comparison facts) (not (returned-p :comparison))) :original-png-target-refusal)
         (insist (eql 0 (data (concatenate 'string *fixture-root* "recorder/pause-01-identify.exit"))) :actual-identify-succeeded)
         (unless (eq kind :geometry)
           (insist (eql 0 (data (concatenate 'string *fixture-root* "recorder/pause-01-decode.exit"))) :actual-full-decode-succeeded))
         (when (member kind '(:pixel :wrong-comparison-phase :omitted-bank))
           (let ((comparison (getf (data (concatenate 'string *fixture-root* "recorder/pause-01-comparison.sexp")) :comparison)))
             (insist (and (null (getf comparison :pass)) (eql 245760 (getf comparison :compared-pixels))
                          (plusp (getf comparison :different-pixels))) :actual-full-comparator-failure)))))
      (t
       (require-prefix :routing :encode :comparison :writer :valid-recorder :recorder-fault)
       (let ((expected
               (if (eq kind :missing-original)
                   (list :file (concatenate 'string *fixture-root* "recorder/RESULT.sexp"))
                   (original-need-facts
                    (ecase kind
                      (:malformed-original "Improper properties") (:trailing-original "Trailing receipt object")
                      ((:unknown-original :duplicate-original) "Unknown/duplicate key")
                      (:wrong-recorder-backend "Recorder identity/status")
                      (:missing-check "Incomplete recorder checks") (:duplicate-check "Bad checks")
                      (:wrong-pause "Duplicate/bad pause") (:wrong-count "Contradictory counts")
                      (:incomplete-recorder "Frame count mismatch") (:close-failure "Invalid recorder PASS")
                      (:comparison-phase "Comparison phase mismatch") (:comparison-facts "Missing key")
                      (:length "Decoded length mismatch"))))))
         (insist (and (at-refusal-p :import expected) (not (returned-p :import))) :original-strict-import-refusal))))))

(defun assert-public (result code positive control)
  (insist (eq :vttest-area-result (getf result :kind)) :actual-public-result)
  (insist (eql code (getf result :exit)) :actual-public-exit)
  (insist (null (getf result :fixture)) :public-fixture-flag-unchanged)
  (if positive
      (progn
        (insist (and (zerop code) (eq :pass (getf result :outcome))) :public-positive)
        (insist (eql 1 (getf result :required-count)) :one-required)
        (insist (eql 1 (getf result :observed-count)) :one-actual-capture)
        (insist (eql 1 (getf result :passed-count)) :one-actual-pass)
        (let* ((evidence (getf result :driver-evidence))
               (stages (getf (getf evidence :counts) :stages)))
          (insist (eql 1 (getf (getf evidence :counts) :captured)) :native-count-projection)
          (insist (eql 1 (getf (getf stages :recorder) :captured)) :recorder-stage)
          (insist (eql 1 (getf (getf stages :native) :capture-completed)) :native-stage)))
      (progn
        (insist (and (not (zerop code)) (member (getf result :outcome) '(:fail :unverified))) :public-negative)
        (let* ((kind (first control)) (evidence (getf result :driver-evidence))
               (counts (getf evidence :counts)) (native (getf evidence :native-record)))
          (when (member kind '(:client-failure :finalization-failure))
            (insist (and (eql 1 code) (eql 1 (getf result :exit))
                         (eq :fail (getf result :outcome)) (getf evidence :native-failed))
                    :public-lifecycle-failure)
            (dolist (projection (list (getf result :counts) counts))
              (insist (and (eql (if (eq kind :client-failure) 1 0) (getf projection :verified))
                           (eql 0 (getf projection :failed))
                           (eql (if (eq kind :client-failure) 0 1) (getf projection :unverified)))
                      :lifecycle-honest-comparison-counts))
            (let ((scenarios (getf result :scenarios)))
              (insist (and (= 1 (length scenarios))
                           (equal "charsets-01" (getf (first scenarios) :id))
                           (eq :unverified (getf (first scenarios) :status))
                           (equal '(:incomplete-or-nonlive) (getf (first scenarios) :reasons)))
                      :lifecycle-exact-nonlive-scenario)))
          (cond
            ((eq kind :client-failure)
             (insist (and (eql 1 (getf result :passed-count))
                          (eql 1 (getf result :observed-count))
                          (eql 1 (getf counts :captured))
                          (eql 1 (getf (getf (getf counts :stages) :recorder) :frame-verified))
                          (eql 1 (getf (getf (getf counts :stages) :native) :capture-completed)))
                     :client-failure-retains-honest-comparison-counts)
             (insist (and (eql 1 (getf result :required-count))
                          (every (lambda (key) (eql 1 (getf counts key))) '(:attempted :reached :captured))
                          (every (lambda (key) (eql 1 (getf (getf (getf counts :stages) :recorder) key)))
                                 '(:attempted :reached :captured :frame-verified))
                          (every (lambda (key) (eql 1 (getf (getf (getf counts :stages) :native) key)))
                                 '(:request-observed :capture-invoked :capture-completed))
                          (= 1 (length (getf native :checks))) (= 1 (length (getf result :scenarios))))
                     :client-complete-separate-stages)
             (insist (and (eq :fail (getf native :status)) (getf evidence :native-failed)
                          (eq :fail (getf result :outcome))
                          (eql 1 (getf (getf native :finalization) :client-exit))
                          (eql 1 (data (concatenate 'string *fixture-root* "native/client.exit")))) :actual-client-failure)
             (insist (equal (getf (getf (getf native :stages) :recorder) :origin)
                            (ref *fixture-root* "recorder/RESULT.sexp")) :client-original-origin)
             (insist (equal '(:pause 1 :pixels :pass :bytes :unverified :timing :pass)
                            (first (getf native :checks))) :client-retained-comparison)
             (insist (every (lambda (scenario)
                              (and (eq :unverified (getf scenario :status))
                                   (member :incomplete-or-nonlive (getf scenario :reasons))))
                            (getf result :scenarios)) :client-scenario-refusal)
             (multiple-value-bind (observations ignored)
                 (vttest-charsets-driver:summarize-native
                  (vttest-charsets-driver:make-manifest (getf native :backend)) native 0 *fixture-root*)
               (declare (ignore ignored))
               (insist (and (= 1 (length observations)) (null (getf (first observations) :fresh)))
                       :actual-original-nonfresh-projection)))
            ((eq kind :finalization-failure)
             (insist (and (eql 0 (getf result :passed-count)) (eql 1 (getf counts :captured))
                          (eql 1 (getf result :observed-count))
                           (equal '(:origin nil :attempted nil :reached nil :captured nil :frame-verified nil)
                                  (getf (getf counts :stages) :recorder))
                          (eql 1 (getf (getf (getf counts :stages) :native) :capture-completed))
                          (eq :timeout (getf (getf native :finalization) :state))
                          (eq :fail (getf native :status))) :timeout-known-native-unknown-recorder))
            ((native-fault-p control)
             (insist (and (null native) (null (getf counts :captured))
                          (null (getf counts :stages)) (eql 0 (getf result :passed-count))
                          (getf evidence :receipt-error)) :strict-rejection-unknown-native-counts))
            (t (insist (and (null (getf result :passed-count)) (null counts))
                       :refusal-before-driver-counts)))))))

(defun one-case (root backend route phase control)
  (let* ((positive (eq :positive (first control))) (directory root)
         (*steps* nil) (*refusals* nil) (*fixture-root* nil) (*boundary* :public-main)
         (*native-refusal-text* nil))
    ;; Caller supplies a unique, already exclusively created case root.
    (let* ((manifest (step-call :valid-manifest
                      (lambda ()
                        (let ((value (vttest-charsets-driver:make-manifest backend)))
                          (vttest-charsets-driver:validate value) value))))
           (external (concatenate 'string directory "external.sexp"))
           (explicit (eq route :explicit))
           (manifest-bytes nil) (invocations 0)
           (original (symbol-function 'vttest-charsets-driver::invoke-native))
           (out (make-string-output-stream)) (err (make-string-output-stream))
           (code nil) (stdout nil) (stderr nil))
      (put (concatenate 'string directory "VALID-MANIFEST.sexp") manifest)
      (setf manifest (mutate-manifest manifest control))
      (when explicit
        (put external manifest)
        ;; Extra legal whitespace proves octet staging, not reprinting EQUAL objects.
        (with-open-file (s external :direction :output :if-exists :append)
          (write-string "  " s) (terpri s))
        (setf manifest-bytes (read-octets external)))
      (when (manifest-control-p control)
        (handler-case
            (step-call :changed-manifest
              (lambda ()
                (handler-bind ((error #'observe-error))
                  (multiple-value-bind (value hash) (vttest-area:read-data external)
                    (declare (ignore hash))
                    (vttest-area:validate-manifest value "character-sets" backend)
                    (vttest-charsets-driver:validate value)))))
          (error () nil)))
      (unwind-protect
           (progn
             (setf (symbol-function 'vttest-charsets-driver::invoke-native)
                   (lambda (actual-backend actual-root actual-path)
                     (incf invocations)
                     ;; For default routing, the original generic writer is authoritative.
                     (let ((bytes (or manifest-bytes (read-octets actual-path))))
                      (handler-bind ((error #'observe-error))
                        (materialize-fixture actual-backend actual-root actual-path bytes phase control)))))
             (let ((*standard-output* out) (*error-output* err))
               (setf code (call-with-environment
                           '(("LISPTERM_VTTEST_EXACT_LABELS" . nil)
                             ("LISPTERM_VTTEST_FILTER" . nil))
                           (lambda ()
                            (handler-bind ((error #'observe-error))
                              (vttest-area::main
                               (append (list "--area" "character-sets" "--backend" backend)
                                       (when explicit (list "--manifest" external))))))))))
        (setf (symbol-function 'vttest-charsets-driver::invoke-native) original))
      (setf stdout (get-output-stream-string out) stderr (get-output-stream-string err))
      (with-open-file (s (concatenate 'string directory "public.stdout") :direction :output :if-exists :error)
        (write-string stdout s))
      (with-open-file (s (concatenate 'string directory "public.stderr") :direction :output :if-exists :error)
        (write-string stderr s))
      (put (concatenate 'string directory "public.exit") code)
      (save-diagnostics directory)
      (let* ((path (public-result-path stdout))
             (result (progn (insist path :public-result-path-present) (public-result path))))
        (when explicit
          (insist (equal (getf result :manifest-sha256) (vttest-charsets-pixels::sha256 manifest-bytes))
                  :public-read-same-external-bytes))
        (assert-public result code positive control)
        (assert-boundary result control)
        (when positive
          (let* ((native (getf (getf result :driver-evidence) :native-record))
                 (run-root (namestring (make-pathname :name nil :type nil :defaults (getf native :manifest))))
                 (recorder (data (concatenate 'string run-root "recorder/RESULT.sexp")))
                 (check (first (getf recorder :checks)))
                 (comparison (data (concatenate 'string run-root (getf (getf check :comparison) :path)))))
            (insist (null (getf result :readiness)) :no-fabricated-readiness)
            (insist (equal (getf result :manifest-sha256) (getf native :manifest-sha256)) :public-native-manifest-bytes)
            (insist (equal (getf (getf (getf native :stages) :recorder) :origin)
                            (ref run-root "recorder/RESULT.sexp")) :original-result-byte-origin)
            (insist (and (member :phase comparison) (eq phase (getf comparison :phase))) :actual-comparison-phase)
            (insist (equal '(:row 24 :column 54 :visible t)
                            (getf (vttest-charsets-oracle:snapshot-metadata
                                   (vttest-charsets-oracle:character-set-snapshot)) :cursor)) :cursor-reference)))
        (insist (= invocations (if (manifest-control-p control) 0 1)) :only-native-substitution)
        (when explicit (insist (equalp manifest-bytes (read-octets external)) :external-original-unchanged))
        (list :backend backend :route route :phase phase :control control
              :result path :exit code :invocations invocations)))))

(defun case-plan ()
  "Return intended cases, not results. No fixtures or tools run here."
  (append
   (loop for backend in '("x11" "wayland") append
     (loop for route in '(:default :explicit) append
       (loop for phase in '(nil t) collect (list backend route phase '(:positive)))))
   (loop for backend in '("x11" "wayland") append
     (loop for control in '(:area :backend :driver :menu :source :archive :digest)
           collect (list backend :explicit nil (list control))))
   (loop for backend in '("x11" "wayland") append
     (loop for control in '(:missing-original :malformed-original :trailing-original :unknown-original
                            :duplicate-original :wrong-recorder-backend :altered-original-bytes
                            :missing-check :duplicate-check :wrong-pause :wrong-count :incomplete-recorder
                            :missing-stage :duplicate-stage :incomplete-native :geometry :profile
                            :missing-phase :nonboolean-phase :unstable-phase :comparison-phase :wrong-comparison-phase
                            :wrong-font-vector :pixel :alpha :length :comparison-facts :native-digest
                            :native-failure :client-failure :build-failure :cleanup-failure :close-failure
                            :finalization-failure :late-import :manifest-after)
           collect (list backend :explicit nil (list control))))
   (loop for set below 5 append
     (loop for bank below 2 append
       (loop for control in '(:missing-bank :unresolved-bank :omitted-bank)
             collect (list "x11" :explicit nil (list control set bank)))))))

(defun run-tests (evidence-root cases)
  "LATER AUTHORIZED ENTRY. Fresh absolute directory required. No tests run on LOAD.
Substitute exactly INVOKE-NATIVE; restore its original binding on all exits.
Requires an EXPLICIT bounded case list and integrated public MAIN. No automatic matrix."
  (insist (and (consp cases) (<= (length cases) 124)
               (every (lambda (case) (member case (case-plan) :test #'equal)) cases))
          :explicit-nonempty-bounded-known-cases)
  (insist (and (stringp evidence-root) (plusp (length evidence-root))
                (char= #\/ (char evidence-root 0))
                (char= #\/ (char evidence-root (1- (length evidence-root))))) :absolute-evidence-root)
  (let* ((root (mkdir evidence-root))
         (*cache-root* (mkdir (concatenate 'string root "images/")))
         (*cache* (make-hash-table :test #'equal)) (*unique-pngs* nil)
         (results nil) (failures 0))
    (loop for (backend route phase control) in cases for index from 0 do
      (let ((case-root (mkdir (format nil "~acase-~3,'0d/" root index))))
        (handler-case
            (push (one-case case-root backend route phase control) results)
          (error (c)
            (incf failures)
            (let ((failure (list :backend backend :route route :phase phase :control control
                                 :control-failed (princ-to-string c))))
              (put (concatenate 'string case-root "CONTROL-FAILURE.sexp") failure)
              (push failure results))))))
    (put (concatenate 'string root "UNIQUE-PNGS-REQUIRING-TWO-VIEWS.sexp") (reverse *unique-pngs*))
    (setf results (nreverse results))
    (put (concatenate 'string root "CONTROL-RESULTS.sexp") results)
    (insist (zerop failures) (list :failed-controls failures :total (length results)))
    (length results)))

(defun run-case (evidence-root backend route phase control)
  "Later single-case scheduling entry; retain its image/command/public artifacts."
  (run-tests evidence-root (list (list backend route phase control))))
