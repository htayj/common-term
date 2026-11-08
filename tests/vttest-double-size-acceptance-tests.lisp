;;;; Public six-hold plumbing controls. Inert until RUN-CASE is called.
;;;; Synthetic status/window/capture inputs are never host-execution evidence.
;;;; Load only after complete selected-source admission; see task376 RESULT.md.
(defpackage #:vttest-double-size-acceptance-tests
  (:use #:cl)
  (:export #:case-names #:run-case))
(in-package #:vttest-double-size-acceptance-tests)

;; Admit this declaration before the selected original recorder PATH/SAVE forms.
;; It supplies the original dynamic binding, not a replacement writer.
(declaim (special vttest-double-size-recorder::*art*))

(defparameter +cases+
  '(:positive :wrong-area :wrong-backend :wrong-driver :wrong-menu :wrong-label
    :wrong-source-pin :wrong-archive-pin :wrong-profile :wrong-env-label :wrong-env-filter
    :missing-original :malformed-original :trailing-original :unknown-original-field
    :duplicate-original-field :missing-original-field :altered-original-bytes
    :missing-pause :duplicate-pause :comparison-subset :declarations-only
    :incomplete-recorder :contradictory-recorder :incomplete-native :contradictory-native
    :wrong-wide-geometry :missing-phase :nonboolean-phase :unstable-phase :hold5-off
    :wrong-font-vector :pixel-mismatch :alpha-mismatch :length-mismatch
    :missing-comparison-facts :native-nonzero :client-exit :build-failure
    :cleanup-failure :finalization-failure :late-import :native-manifest-pin
    :altered-manifest-bytes))
(defvar *case*)
(defvar *evidence*)
(defvar *cache*)
(defvar *calls*)
(defvar *comparisons*)
(defvar *step*)
(defvar *invocation-root*)
(defvar *refusal*)
(defvar *completed*)

(defun observed-boundary (boundary function)
  "Observe original conditions without handling them or changing function cells."
  (setf *step* boundary)
  (handler-bind ((error (lambda (c) (setf *refusal* (cons boundary c)))))
    (multiple-value-prog1 (funcall function) (push boundary *completed*))))

(defun simple-refusal-p (condition reason)
  ;; Compare the original SIMPLE-ERROR format datum, not rendered prose. In
  ;; particular, undefined functions, file errors and command failures differ.
  (and (typep condition 'simple-error)
       (or (and (equal (simple-condition-format-control condition) "~a")
                (equal (simple-condition-format-arguments condition) (list reason)))
           (and (equal (simple-condition-format-control condition) reason)
                (null (simple-condition-format-arguments condition))))))

(defun refusal-facts ()
  (when *refusal*
    (let ((c (cdr *refusal*)) (*print-length* 8) (*print-level* 4))
      (list :boundary (car *refusal*) :type (princ-to-string (type-of c))
        :frame-reasons (when (typep c 'vttest-double-size-native-contract:frame-refusal)
                         (vttest-double-size-native-contract:refusal-reasons c))
        :profile-field (when (typep c 'vttest-double-size-oracle:profile-error)
                         (vttest-double-size-oracle:profile-error-field c))
        :format-control (when (typep c 'simple-condition)
                          (princ-to-string (simple-condition-format-control c)))
        :message (princ-to-string c)))))

(defun case-names () (copy-list +cases+))
(defun insist (truth context)
  (unless truth (error "Double-size public control failed: ~s" context)))
(defun data (path value)
  (vttest-double-size-native-contract:write-data path value))
(defun read-data (path)
  (vttest-double-size-native-contract:read-data path))
(defun digest (path) (vttest-double-size-native-contract::digest path))
(defun bytes (path) (vttest-double-size-pixels::octets path))
(defun write-bytes (path octets)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :error :if-does-not-exist :create)
    (write-sequence octets s)))
(defun copy-bytes (from to) (write-bytes to (bytes from)))
(defun cat (root relative) (concatenate 'string root relative))
(defun stem (n) (format nil "recorder/pause-~2,'0d-" n))
(defun directory! (path)
  ;; Every directory this test creates is exclusive. No repair or deletion.
  (sb-posix:mkdir path #o700)
  (namestring (truename (pathname (concatenate 'string path "/")))))
(defun append-text (path text)
  ;; Used only for named corruption controls, after retaining the writer bytes.
  (with-open-file (s path :direction :output :if-exists :append)
    (write-string text s)))
(defun change-data (path function)
  ;; Preserve original bytes before a deliberately invalid replacement.
  (copy-bytes path (concatenate 'string path ".before-corruption"))
  (let ((value (funcall function (read-data path))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (let ((*print-pretty* nil) (*print-readably* t))
        (write value :stream s) (terpri s)))))
(defun with-environment (bindings function)
  (let ((saved (mapcar (lambda (b) (cons (car b) (sb-ext:posix-getenv (car b)))) bindings)))
    (unwind-protect
         (progn (dolist (b bindings)
                  (if (cdr b) (sb-posix:setenv (car b) (cdr b) 1)
                      (sb-posix:unsetenv (car b))))
                (funcall function))
      (dolist (b saved)
        (if (cdr b) (sb-posix:setenv (car b) (cdr b) 1)
            (sb-posix:unsetenv (car b)))))))

(defun reference-png (n)
  "Create each of six immutable synthetic reference PNG payloads once per cache.
Actual encoder execution uses the original retained-command helper. No callback.
Cache custody and reuse belong to this test invocation, not native acceptance."
  (let* ((base (format nil "hold-~2,'0d" n))
         (raw (cat *cache* (concatenate 'string base ".rgba")))
         (png (cat *cache* (concatenate 'string base ".png")))
         (receipt (cat *cache* (concatenate 'string base ".seal.sexp"))))
    (if (probe-file receipt)
        (let ((r (read-data receipt)))
          (insist (and (equal (getf r :raw) (digest raw))
                       (equal (getf r :png) (digest png))) :cache-bytes))
        (let* ((snapshot (vttest-double-size-oracle:reference n))
               (font (bytes vttest-double-size-pixels::+font+))
               (sha (vttest-double-size-pixels::font-digest font))
               (rgba (progn
                       (insist (equal sha vttest-double-size-oracle:+font-sha256+) :font-vector)
                       (vttest-double-size-oracle:rasterize snapshot font :blink-phase (= n 5)
                                                           :verified-font-sha256 sha)))
               (width (* 8 (vttest-double-size-oracle:snapshot-columns snapshot))))
          (insist (equal (digest vttest-double-size-pixels::+decoder+)
                         vttest-double-size-pixels::+decoder-sha+) :encoder-pin)
          (write-bytes raw rgba)
          (vttest-double-size-pixels::command *cache* (concatenate 'string base "-encode")
            (list vttest-double-size-pixels::+decoder+ "-size" (format nil "~dx384" width)
                  "-depth" "8" (concatenate 'string "rgba:" raw) (concatenate 'string "PNG32:" png)))
          (data receipt (list :kind :synthetic-reference-png :pause n :phase (= n 5)
                              :font-vector-sha256 sha :raw (digest raw) :png (digest png)))))
    png))

(defun assert-reference-profile (snapshot n)
  (let* ((metadata (vttest-double-size-oracle:snapshot-metadata snapshot))
         (cursor (getf metadata :cursor))
         (blinking 0))
    (insist (= (vttest-double-size-oracle:snapshot-columns snapshot)
               (if (member n '(3 4)) 132 80)) :reference-width)
    (insist (and (= (getf cursor :row) (if (= n 6) 1 23))
                 (= (getf cursor :column) (case n (5 54) (6 53) (t 14)))) :reference-cursor)
    (loop for row from 1 to 24 do
      (loop for column from 1 to
          (vttest-double-size-oracle:logical-columns
            (vttest-double-size-oracle:snapshot-columns snapshot)
            (vttest-double-size-oracle:snapshot-row-mode snapshot row))
        for cell = (vttest-double-size-oracle:snapshot-cell snapshot row column) do
        (insist (eq :single (vttest-screen-oracle:cell-line-size cell)) :area-local-row-mode)
        (when (vttest-screen-oracle:cell-blink cell) (incf blinking))))
    (insist (= blinking (case n (5 73) (6 37) (t 0))) :actual-blinking-cell-coverage)))

(defun fixture-frame (n backend)
  "Explicit synthetic public observations, not observations of a server/window."
  (let* ((columns (if (member n '(3 4)) 132 80)) (width (* columns 8))
         (phase (= n 5)) (content (not (null (member n '(5 6)))))
         (g (list :phase :settled :request-id n :display-epoch 1 :epoch n
                  :actual-width width :actual-height 384 :cell-width 8 :cell-height 16
                  :presentation 2 :target-presentation 2 :rows 24 :columns columns
                  :window-id 7 :frame-pending-p nil :commit-generation 4 :frame-generation 4))
         (before (list :id 1 :child-pid 2 :backend backend :rows 24 :columns columns
                       :pty-rows 24 :pty-columns columns :geometry g :frames 10
                       :blink-phase phase :blink-visible content :bytes-received 100
                       :input-pending 0 :error-count 0 :process-state :running :transport-state :open))
         (ready (copy-tree before)) (ack (if (eq backend :x11) -1 100))
         (native (list :version 3 :backend backend :pause n :window-id 7 :native-window-id 7
                       :surface-id 8 :width width :height 384 :scale 1 :capture-count 1
                       :owned t :capture-exit 0 :png (format nil "pause-~2,'0d-actual.png" n)
                       :ack-output ack :ack-basis :recorder-request-binding :custody t)))
    (setf (getf ready :frames) 11)
    (when (eq backend :wayland)
      (setf (getf (getf ready :geometry) :commit-generation) 5
            (getf (getf ready :geometry) :frame-generation) 5))
    (let ((pre (copy-tree ready)) (final (copy-tree ready)))
      (when (= n 5)
        (case *case*
          (:wrong-wide-geometry (setf (getf native :width) 1056))
          (:missing-phase (remf ready :blink-phase))
          (:nonboolean-phase (setf (getf ready :blink-phase) :unknown))
          (:unstable-phase (setf (getf pre :blink-phase) nil))
          (:hold5-off (dolist (s (list before ready pre final)) (setf (getf s :blink-phase) nil)))))
      (list before ready pre final native ack))))

(defun prepare-pause (root n backend)
  (destructuring-bind (before ready pre final native ack) (fixture-frame n backend)
    (let* ((prefix (stem n)) (snapshot (vttest-double-size-oracle:reference n))
           (png (cat root (cat prefix "actual.png"))))
      (assert-reference-profile snapshot n)
      (loop for value in (list before ready pre final native)
            for suffix in '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp")
            do (data (cat root (cat prefix suffix)) value))
      (data (cat root (cat prefix "request.sexp"))
        (list :pause n :target 100 :status ready :ack-output ack :surface-id 8))
      ;; Marker and stage rows are synthetic native-invocation inputs, explicitly
      ;; labeled in FIXTURE-INPUTS.sexp. No capture argv/exit is asserted to be real.
      (data (cat root (cat prefix "request")) :synthetic-request-marker)
      (data (cat root (cat prefix "capture.exit")) 0)
      (cond
        ((and (= n 1) (member *case* '(:wrong-font-vector :length-mismatch)))
         (let* ((font (bytes vttest-double-size-pixels::+font+))
                (sha (vttest-double-size-pixels::font-digest font))
                (rgba (vttest-double-size-oracle:rasterize snapshot font :blink-phase nil
                                                          :verified-font-sha256 sha)))
           (if (eq *case* :wrong-font-vector) (setf (aref font 0) (logxor 1 (aref font 0)))
               (setf rgba (subseq rgba 0 (1- (length rgba)))))
           (setf *step* :corrupt-memory-comparison)
            (observed-boundary :corrupt-memory-comparison
              (lambda () (vttest-double-size-pixels:compare-memory snapshot rgba font nil)))
           (setf *step* :unexpected-corrupt-memory-acceptance)
           (error "Corrupt font/length unexpectedly accepted")))
        ((and (= n 1) (member *case* '(:pixel-mismatch :alpha-mismatch)))
         (reference-png n)
         (let* ((rgba (bytes (cat *cache* "hold-01.rgba")))
                (raw (cat root (cat prefix "encoder-input.rgba"))))
           (if (eq *case* :pixel-mismatch) (setf (aref rgba 0) (logxor 1 (aref rgba 0)))
               (setf (aref rgba 3) 254))
           (write-bytes raw rgba)
           (vttest-double-size-pixels::command root (cat prefix "encode")
             (list vttest-double-size-pixels::+decoder+ "-size" "640x384" "-depth" "8"
                   (cat "rgba:" raw) (cat "PNG32:" png)))))
        (t (copy-bytes (reference-png n) png)))
      (setf *step* (list :compare-capture n))
      (incf (aref *comparisons* (1- n)))
      (let ((comparison (observed-boundary (list :compare-capture n)
                          (lambda () (vttest-double-size-pixels:compare-capture
                            root n snapshot before ready pre final native 100 ack 8)))))
        (when (and (= n 1) (eq *case* :missing-comparison-facts))
          (change-data (cat root (getf comparison :path))
                       (lambda (r) (remf (getf r :comparison) :masked-pixels) r))
          (setf comparison (vttest-double-size-native-contract:reference root (getf comparison :path))))
        (list :pause n :pixels :pass :bytes :unverified :timing :pass :frame :pass
              :target :recognized-navigation-only :native native
              :evidence (format nil "pause-~2,'0d-actual.png" n)
              :reason "Synthetic fixture inputs; no native capture executed"
              :comparison comparison)))))

(defun recorder-record (backend checks)
  (let ((r (list :kind :vttest-double-size-run :version 2 :backend backend :status :pass
                 :attempted 6 :reached 6 :captured 6 :frame-verified 6 :bytes :unverified
                 :wire-accounting :unverified :reason "Synthetic public plumbing fixture only"
                 :cleanup :graceful-exit :checks checks
                 :evidence '("events.sexp" "output.raw" "input.raw" "application.log" "launch.sexp" "terminal-status.sexp"))))
    (case *case*
      (:malformed-original (setf r "Not a recorder record"))
      (:unknown-original-field (setf r (append r (list :unknown t))))
      (:duplicate-original-field (setf r (append r (list :status :pass))))
      (:missing-original-field (remf r :bytes))
      (:missing-pause (setf (getf r :checks) (subseq checks 0 5)))
      (:duplicate-pause (setf (getf (second checks) :pause) 1))
      (:comparison-subset (setf (getf (sixth checks) :pixels) :unverified
                               (getf r :status) :unverified))
      (:incomplete-recorder (setf (getf r :captured) 5))
      (:contradictory-recorder (setf (getf r :attempted) 5)))
    r))

(defun publish-fixture (root backend checks)
  (setf *step* :writer)
  (unless (eq *case* :missing-original)
    ;; These are the actual selected original PATH/SAVE definitions, never MAIN.
    (let ((vttest-double-size-recorder::*art* (pathname (cat root "recorder/"))))
      (vttest-double-size-recorder::save "RESULT.sexp" (recorder-record backend checks)))
    (copy-bytes (cat root "recorder/RESULT.sexp") (cat root "recorder/WRITER-RESULT.original"))
    (when (eq *case* :trailing-original) (append-text (cat root "recorder/RESULT.sexp") "0")))
  (push :writer-complete *completed*)
  (setf *step* :original-import)
  (observed-boundary :original-import
    (lambda () (vttest-double-size-native-contract:import-recorder root backend)))
  (let ((origin (getf (read-data (cat root "native/recorder-import.sexp")) :origin)))
    (insist (equal origin (vttest-double-size-native-contract:reference root "recorder/RESULT.sexp")) :original-byte-origin))
  (data (cat root "native/client.exit") (if (eq *case* :client-exit) 9 0))
  ;; One explicit synthetic clock and original import record, same 15s limit.
  ;; They model finalization inputs; no PID was polled and no process exited.
  (data (cat root "native/finalization-clock.sexp")
    (list :clock :clock-monotonic :limit-seconds 15 :start-ns 1000000000 :end-ns 2000000000
          :state :exited :pid 2 :token "synthetic-not-a-process" :run root :backend backend))
  (data (cat root "native/import-deadline.sexp")
    (list :clock :clock-monotonic :deadline-ns 16000000000
          :validated-ns (if (eq *case* :late-import) 16000000000 3000000000) :state :valid))
  (with-open-file (s (cat root "native/unowned-endpoints.txt") :direction :output :if-exists :error)
    (finish-output s))
  (with-open-file (s (cat root "native/stage-events.sexp") :direction :output :if-exists :error)
    (loop for n from 1 to 6 do
      (dolist (row (list (list n :request) (list n :invoked) (list n :completed 0)))
        (write row :stream s) (terpri s))))
  (setf *step* :native-publication)
  (with-environment
      (loop for (key value) on
        (list "ROOT" root "BACKEND" backend
              "FINALIZATION" (if (eq *case* :finalization-failure) ":timeout" ":complete")
              "CLIENT_EXIT" (if (eq *case* :client-exit) "9" "0") "STATUS" ":pass"
              "ATTEMPTED" "6" "REACHED" "6" "CAPTURED" "6"
              "BUILD" (if (eq *case* :build-failure) ":fail" ":pass") "STARTED" "T"
              "CLEANUP" (if (eq *case* :cleanup-failure) ":fail" ":pass")
              "REASON" "Synthetic native publication inputs, not native acceptance")
        by #'cddr collect (cons (cat "LISPTERM_DOUBLE_SIZE_" key) value))
    (lambda () (observed-boundary :native-publication
                 #'vttest-double-size-native-contract:publish-native)))
  (when (member *case* '(:altered-original-bytes :incomplete-native :contradictory-native))
    (mutate-validated-native root backend))
  (when (eq *case* :native-manifest-pin)
    (change-data (cat root "DOUBLE-SIZE-RUN.sexp")
      (lambda (r) (setf (getf r :manifest-sha256) (make-string 64 :initial-element #\0)) r)))
  (when (eq *case* :altered-manifest-bytes)
    (copy-bytes (cat root "MANIFEST.sexp") (cat *evidence* "NATIVE-MANIFEST.before-corruption"))
    (append-text (cat root "MANIFEST.sexp") " ")))

(defun native-fault-value (record root)
  (let* ((r (copy-tree record)) (n (getf (getf r :stages) :native)))
    (case *case*
      (:incomplete-native
       (setf (getf n :origin) (vttest-double-size-native-contract:reference root "native/stages.sexp"))
       (dolist (k '(:request-observed :capture-invoked :capture-completed)) (setf (getf n k) 5))
       (dolist (k '(:attempted :reached :captured)) (setf (getf r k) 5)))
      (:contradictory-native (setf (getf n :capture-invoked) 5)))
    r))

(defun mutate-validated-native (root backend)
  "Retain a genuinely valid original publication, then only the named fault.
  The supplemental validator never replaces the driver's original validation."
  (let* ((path (cat root "DOUBLE-SIZE-RUN.sexp")) (baseline (read-data path)))
    (observed-boundary :baseline-native-validation
      (lambda () (vttest-double-size-native-contract:validate-native baseline backend root)))
    (copy-bytes path (cat root "native/VALID-NATIVE.original"))
    (copy-bytes (cat root "native/stages.sexp") (cat root "native/VALID-LEDGER.original"))
    (case *case*
      (:altered-original-bytes (append-text (cat root "recorder/RESULT.sexp") " "))
      (:incomplete-native
       (change-data (cat root "native/stages.sexp")
         (lambda (r) (setf (getf r :entries) (subseq (getf r :entries) 0 5)) r))))
    (unless (eq *case* :altered-original-bytes)
      (change-data path (lambda (r) (native-fault-value r root))))
    (data (cat root "native/MUTATION.sexp")
      (list :case *case* :baseline (digest (cat root "native/VALID-NATIVE.original"))
            :changed (digest path) :ledger (digest (cat root "native/stages.sexp"))
            :recorder (digest (cat root "recorder/RESULT.sexp"))))
    (handler-case
        (progn (observed-boundary :mutated-native-validation
                 (lambda () (vttest-double-size-native-contract:validate-native (read-data path) backend root)))
               (error "Native mutation unexpectedly accepted"))
      (error () nil))))

(defun native-fixture (backend root manifest-path)
  (incf *calls*) (insist (= *calls* 1) :once-only-invocation)
  (setf *invocation-root* root *step* :native-entry)
  (insist (equal manifest-path (cat root "MANIFEST.sexp")) :native-manifest-location)
  (let* ((binding (read-data (cat root "SOURCE-BINDING-PRE.sexp")))
         (original (getf binding :manifest)))
    (insist (and (equal (digest original) (getf binding :manifest-sha256))
                 (equalp (bytes original) (bytes manifest-path))) :exact-manifest-octets)
    (copy-bytes manifest-path (cat *evidence* "NATIVE-MANIFEST.original"))
    (data (cat *evidence* "STAGING.sexp")
      (list :source original :destination manifest-path :source-sha256 (digest original)
            :destination-sha256 (digest manifest-path) :same-path (equal original manifest-path))))
  (directory! (cat root "recorder")) (directory! (cat root "native"))
  (data (cat root "FIXTURE-INPUTS.sexp")
    (list :kind :synthetic-double-size-public-plumbing :case *case* :backend backend
          :host-execution nil :capture-execution nil :fixture-phase-policy '(:hold5 t :others nil)
          :status-origin :explicit-test-data :stage-event-origin :explicit-test-data
          :capture-exit-origin :explicit-test-data :clock-origin :explicit-test-data))
  (if (eq *case* :declarations-only)
      (progn (push :declarations-only *completed*) 0)
      (progn (publish-fixture root backend
               (loop for n from 1 to 6 collect
                 (prepare-pause root n (intern (string-upcase backend) :keyword))))
            (push :native-return *completed*)
            (if (eq *case* :native-nonzero) 7 0))))

(defun routing-case-p (case)
  (member case '(:wrong-area :wrong-backend :wrong-driver :wrong-menu :wrong-label
                :wrong-source-pin :wrong-archive-pin :wrong-profile :wrong-env-label :wrong-env-filter)))
(defun mutate-manifest (manifest)
  (case *case*
    (:wrong-area (setf (getf manifest :area) "cursor-movements"))
    (:wrong-backend (setf (getf manifest :backend) "foreign-backend"))
    (:wrong-driver (setf (getf manifest :native-driver) "screen-x11-v1"))
    (:wrong-menu (setf (getf (first (getf manifest :scenarios)) :menu-path) '(2)))
    (:wrong-label (setf (getf (first (getf manifest :scenarios)) :recorder-label) "screen"))
    (:wrong-source-pin (setf (getf (second (getf manifest :pins)) :sha256) (make-string 64 :initial-element #\0)))
    (:wrong-archive-pin (setf (getf (first (getf manifest :pins)) :sha256) (make-string 64 :initial-element #\0)))
    (:wrong-profile (setf (getf manifest :conditions) '("wrong-entry-profile"))))
  manifest)

(defun assert-completed-comparisons (count)
  (insist (equal (loop for n from 1 to 6 collect
                  (count (list :compare-capture n) *completed* :test #'equal))
                (loop for n from 1 to 6 collect (if (<= n count) 1 0)))
          :original-comparisons-returned))

(defun assert-original-fault ()
  "Authenticate the intended original writer payload, not just a step flag."
  (let* ((path (cat *invocation-root* "recorder/RESULT.sexp"))
         (saved (cat *invocation-root* "recorder/WRITER-RESULT.original"))
         (r (unless (eq *case* :missing-original) (read-data saved))))
    (insist (case *case*
      (:missing-original (and (not (probe-file path)) (not (probe-file saved))))
      (:malformed-original (equal r "Not a recorder record"))
      (:trailing-original (equalp (bytes path)
                                 (concatenate '(vector (unsigned-byte 8)) (bytes saved) #(48))))
      (:unknown-original-field (eq t (getf r :unknown)))
      (:duplicate-original-field (= 2 (count :status r)))
      (:missing-original-field (not (member :bytes r)))
      (:missing-pause (= 5 (length (getf r :checks))))
      (:duplicate-pause (eql 1 (getf (second (getf r :checks)) :pause)))
      (:incomplete-recorder (and (eql 5 (getf r :captured)) (eql 6 (getf r :frame-verified))))
      (:contradictory-recorder (and (eql 5 (getf r :attempted)) (eql 6 (getf r :reached))))
      (:missing-comparison-facts
       (let* ((ref (getf (first (getf r :checks)) :comparison))
              (c (read-data (cat *invocation-root* (getf ref :path)))))
         (and (not (member :masked-pixels (getf c :comparison)))
              (equal (getf ref :sha256) (digest (cat *invocation-root* (getf ref :path)))))))
      (otherwise nil)) :intended-original-writer-fault)
    (unless (member *case* '(:missing-original :trailing-original))
      (insist (equalp (bytes path) (bytes saved)) :writer-bytes-at-original-refusal))))

(defun assert-negative-boundary (record)
  (let* ((evidence (getf record :driver-evidence))
         (c (cdr *refusal*)) (boundary (car *refusal*))
         (native (getf evidence :native-record)))
    (cond
      ((routing-case-p *case*)
       (insist (and (zerop *calls*) (null *refusal*)
                    (every #'zerop *comparisons*) (eql 2 (getf record :exit))
                    (eq (getf record :reason)
                        (case *case*
                          ((:wrong-area :wrong-backend) :manifest-selector)
                          ((:wrong-menu :wrong-label) :recorder-selector)
                          ((:wrong-env-label :wrong-env-filter) :invalid-recorder-selection)
                          (:wrong-driver :driver-not-integrated)
                          (otherwise :double-size-manifest-mismatch))))
               :specific-public-routing-refusal))
      (t
       (insist (= *calls* 1) :negative-crossed-native-boundary)
       (cond
         ((member *case* '(:wrong-font-vector :length-mismatch))
          (assert-completed-comparisons 0)
          (insist (and (every #'zerop *comparisons*)
                       (eq boundary :corrupt-memory-comparison)
                       (if (eq *case* :wrong-font-vector)
                           (simple-refusal-p c "Font vector authentication refusal")
                           (and (typep c 'vttest-double-size-oracle:profile-error)
                                (eq :rgba (vttest-double-size-oracle:profile-error-field c))
                                (= (length (vttest-double-size-oracle:profile-error-value c))
                                   (1- (* 640 384 4)))))) :typed-memory-refusal))
         ((member *case* '(:wrong-wide-geometry :missing-phase :nonboolean-phase :unstable-phase :hold5-off))
          (assert-completed-comparisons 4)
          (insist (and (equal boundary '(:compare-capture 5))
                       (equal (coerce *comparisons* 'list) '(1 1 1 1 1 0))
                       (typep c 'vttest-double-size-native-contract:frame-refusal)
                       (member (case *case*
                                 (:wrong-wide-geometry :width)
                                 (:missing-phase '(:missing :blink-phase))
                                 (:nonboolean-phase '(:boolean :blink-phase))
                                 (:unstable-phase :ready-pre)
                                 (:hold5-off :pause5-blink-on))
                               (vttest-double-size-native-contract:refusal-reasons c) :test #'equal))
                  :specific-original-frame-refusal))
         ((member *case* '(:pixel-mismatch :alpha-mismatch))
          (assert-completed-comparisons 0)
          (insist (and (equal boundary '(:compare-capture 1))
                       (equal (coerce *comparisons* 'list) '(1 0 0 0 0 0))
                       (if (eq *case* :alpha-mismatch)
                           (and (typep c 'vttest-double-size-oracle:profile-error)
                                (eq :rgba (vttest-double-size-oracle:profile-error-field c))
                                (= 254 (aref (vttest-double-size-oracle:profile-error-value c) 3)))
                           (and (typep c 'simple-error)
                                (equal (simple-condition-format-control c)
                                       "Exact double-size raster mismatch at pause ~d")
                                (equal (simple-condition-format-arguments c) '(1))
                                (let ((r (read-data (cat *invocation-root* "recorder/pause-01-comparison.sexp"))))
                                  (and (null (getf (getf r :comparison) :pass))
                                       (eql 1 (getf (getf r :comparison) :different-pixels)))))))
                  :actual-decoded-pixel-refusal))
         ((eq *case* :declarations-only)
          (assert-completed-comparisons 0)
          (insist (and (member :declarations-only *completed*) (null *refusal*)
                       (eql 0 (getf evidence :native-exit)) (null native)
                       (null (getf evidence :receipt-read-error))
                       (eq :incomplete-or-failed (getf record :reason))) :only-declarations-reached))
         (t
          (assert-completed-comparisons 6)
          (insist (equal (coerce *comparisons* 'list) '(1 1 1 1 1 1)) :once-only-comparison-attempts)
          (insist (member :writer-complete *completed*) :original-writer-reached)
          (cond
            ((member *case* '(:missing-original :malformed-original :trailing-original
                              :unknown-original-field :duplicate-original-field :missing-original-field
                              :missing-pause :duplicate-pause :incomplete-recorder :contradictory-recorder
                              :missing-comparison-facts))
             (assert-original-fault)
             (insist (and (eq boundary :original-import)
                          (not (member :original-import *completed*))
                          (if (eq *case* :missing-original)
                              (and (typep c 'file-error)
                                   (equal (namestring (file-error-pathname c))
                                          (cat *invocation-root* "recorder/RESULT.sexp")))
                              (simple-refusal-p c
                                (case *case*
                                  (:malformed-original "Improper properties")
                                  (:trailing-original "Trailing receipt object")
                                  ((:unknown-original-field :duplicate-original-field) "Unknown/duplicate key")
                                  ((:missing-original-field :missing-comparison-facts) "Missing key")
                                  (:missing-pause "Incomplete recorder checks")
                                  (:duplicate-pause "Duplicate/bad pause")
                                  (otherwise "Contradictory counts"))))) :specific-strict-original-refusal))
            (t
             (insist (member :original-import *completed*) :original-import-returned)
             (case *case*
               ((:altered-original-bytes :incomplete-native :contradictory-native)
                (let* ((root *invocation-root*)
                       (baseline (read-data (cat root "native/VALID-NATIVE.original")))
                       (ledger (read-data (cat root "native/VALID-LEDGER.original")))
                       (reason (case *case*
                                 (:altered-original-bytes "Reference hash mismatch")
                                 (:contradictory-native "Contradictory counts")
                                 (otherwise "Native contract refusal"))))
                  (when (eq *case* :incomplete-native)
                    (setf (getf ledger :entries) (subseq (getf ledger :entries) 0 5)))
                  (insist (and (member :native-publication *completed*)
                               (member :baseline-native-validation *completed*)
                               (member :native-return *completed*)
                               (eq boundary :mutated-native-validation)
                               (not (member :mutated-native-validation *completed*))
                               (simple-refusal-p c reason) (null native)
                               (null (getf evidence :receipt-read-error))
                               (equal reason (getf evidence :receipt-error))
                               (eq :incomplete-or-failed (getf record :reason))
                               (equal (native-fault-value baseline root)
                                      (read-data (cat root "DOUBLE-SIZE-RUN.sexp")))
                               (equal ledger (read-data (cat root "native/stages.sexp")))
                               (equalp (bytes (cat root "recorder/RESULT.sexp"))
                                 (if (eq *case* :altered-original-bytes)
                                     (concatenate '(vector (unsigned-byte 8))
                                       (bytes (cat root "recorder/WRITER-RESULT.original")) #(32))
                                     (bytes (cat root "recorder/WRITER-RESULT.original")))))
                          :baseline-sole-mutation-and-original-driver-refusal)))
               ((:comparison-subset :build-failure :cleanup-failure :late-import)
                (insist (and (eq boundary :native-publication)
                             (not (member :native-publication *completed*))
                             (simple-refusal-p c (case *case*
                               (:comparison-subset "Invalid PASS")
                               (:late-import "Late import")
                               (otherwise "Failure downgraded")))) :specific-native-publication-refusal))
               (otherwise
                (insist (and (null *refusal*) (member :native-publication *completed*)
                             (member :native-return *completed*)
                             (eq :incomplete-or-failed (getf record :reason))) :publication-before-driver-fault)
                (case *case*
                  (:native-manifest-pin
                   (insist (and (null native)
                                (search "DOUBLE-SIZE-RECEIPT-MANIFEST-MISMATCH"
                                        (string-upcase (or (getf evidence :receipt-read-error) "")))
                                (equal (getf (read-data (cat *invocation-root* "DOUBLE-SIZE-RUN.sexp")) :manifest-sha256)
                                       (make-string 64 :initial-element #\0))) :specific-receipt-pin-refusal))
                  (:altered-manifest-bytes
                   (insist (and (search "DOUBLE-SIZE-MANIFEST-CHANGED"
                                        (string-upcase (or (getf evidence :receipt-read-error) "")))
                                (equalp (bytes (cat *invocation-root* "MANIFEST.sexp"))
                                  (concatenate '(vector (unsigned-byte 8))
                                    (bytes (cat *evidence* "NATIVE-MANIFEST.before-corruption")) #(32))))
                           :specific-staged-byte-refusal))
                  (otherwise
                   (insist (and native (getf evidence :native-failed)
                                (null (getf evidence :receipt-read-error))
                                (every (lambda (k) (eql 6 (getf (getf (getf native :stages) :native) k)))
                                       '(:request-observed :capture-invoked :capture-completed))
                                (if (eq *case* :finalization-failure)
                                    (null (getf (getf (getf native :stages) :recorder) :origin))
                                    (every (lambda (k) (eql 6 (getf (getf (getf native :stages) :recorder) k)))
                                           '(:attempted :reached :captured :frame-verified)))
                                (case *case*
                                  (:native-nonzero (eql 7 (getf evidence :native-exit)))
                                  (:client-exit (eql 9 (getf (getf native :finalization) :client-exit)))
                                  (:finalization-failure (eq :timeout (getf (getf native :finalization) :state)))
                                  (otherwise nil))) :specific-published-lifecycle-failure)))))))))))))

(defun public-result-path (stdout)
  (let* ((prefix "RESULT ") (at (search prefix stdout)))
    (insist at :actual-public-writer-announcement)
    (let* ((start (+ at (length prefix))) (end (or (position #\Newline stdout :start start) (length stdout))))
      (string-trim '(#\Return #\Space) (subseq stdout start end)))))
(defun assert-public-lifecycle (record exit)
  (let* ((evidence (getf record :driver-evidence))
         (receipt (getf evidence :native-record))
         (counts (getf record :counts)) (stages (getf counts :stages))
         (native (getf stages :native)) (recorder (getf stages :recorder))
         (timeout (eq *case* :finalization-failure))
         (passed (if timeout 0 6)) (unverified (if timeout 6 0)))
    (insist (and (eql 1 exit) (eq :fail (getf record :outcome))
                 (eq :incomplete-or-failed (getf record :reason))
                 (getf evidence :native-failed) receipt
                 (eql 6 (getf record :required-count))
                 (eql 6 (getf record :observed-count))
                 (eql passed (getf record :passed-count))
                 (eql 0 (getf record :failed-count))
                 (eql unverified (getf record :unverified-count))
                 (equal counts (getf evidence :counts))
                 (equal stages (getf receipt :stages))
                 (every (lambda (k) (eql 6 (getf counts k)))
                        '(:required :attempted :reached :captured))
                 (eql passed (getf counts :passed))
                 (eql passed (getf counts :verified))
                 (eql 0 (getf counts :failed))
                 (eql unverified (getf counts :unverified)))
            :honest-public-lifecycle-counts)
    (insist (and (getf native :origin)
                 (every (lambda (k) (eql 6 (getf native k)))
                        '(:request-observed :capture-invoked :capture-completed))
                 (if timeout
                     (equal recorder '(:origin nil :attempted nil :reached nil
                                       :captured nil :frame-verified nil))
                     (and (getf recorder :origin)
                          (every (lambda (k) (eql 6 (getf recorder k)))
                                 '(:attempted :reached :captured :frame-verified)))))
            :honest-public-lifecycle-stages)
    (let ((scenarios (getf record :scenarios)))
      (insist (and (= 6 (length scenarios))
                   (equal (mapcar (lambda (s) (getf s :id)) scenarios)
                          (mapcar (lambda (s) (getf s :id))
                                  (getf (vttest-double-size-driver:make-manifest
                                         (getf record :backend)) :scenarios)))
                   (every (lambda (s)
                            (and (eq :unverified (getf s :status))
                                 (equal '(:incomplete-or-nonlive) (getf s :reasons))))
                          scenarios))
              :all-six-public-lifecycle-scenarios-nonlive))))

(defun assert-public (record exit explicit)
  (insist (and (eq (getf record :kind) :vttest-area-result)
               (eql exit (getf record :exit))) :actual-public-result-and-exit)
  (if (eq *case* :positive)
      (progn
        (insist (and (zerop exit) (eq :pass (getf record :outcome)) (= *calls* 1)
                     (equal (coerce *comparisons* 'list) '(1 1 1 1 1 1))
                     (eql 6 (getf record :required-count)) (eql 6 (getf record :observed-count))
                     (eql 6 (getf record :passed-count)) (eql 0 (getf record :failed-count))
                     (eql 0 (getf record :unverified-count))) :complete-public-positive)
        (let* ((counts (getf record :counts)) (stages (getf counts :stages))
               (native (getf stages :native)) (recorder (getf stages :recorder))
               (staging (read-data (cat *evidence* "STAGING.sexp"))))
          (insist (and (every (lambda (k) (eql 6 (getf counts k)))
                              '(:required :attempted :reached :captured :verified :passed))
                       (every (lambda (k) (eql 6 (getf recorder k)))
                              '(:attempted :reached :captured :frame-verified))
                       (every (lambda (k) (eql 6 (getf native k)))
                              '(:request-observed :capture-invoked :capture-completed))
                       (getf recorder :origin) (getf native :origin)) :distinct-complete-stages)
          (insist (eq (not (null (getf staging :same-path))) (not explicit)) :default-versus-external)
          (insist (and (equal (getf staging :source-sha256) (getf staging :destination-sha256))
                       (equal (digest (getf staging :source)) (getf record :manifest-sha256))
                       (equal (digest (getf staging :destination)) (getf record :manifest-sha256))) :final-manifest-bindings))
        (let ((ready6 (read-data (cat *invocation-root* "recorder/pause-06-ready.sexp"))))
          (insist (and (member :blink-phase ready6) (null (getf ready6 :blink-phase))) :valid-hold6-nil)))
      (progn
        (insist (and (not (zerop exit)) (member (getf record :outcome) '(:fail :unverified))) :public-refusal)
        (assert-negative-boundary record)
        (when (member *case* '(:native-nonzero :client-exit :finalization-failure))
          (assert-public-lifecycle record exit))
        (when (and *refusal* (not (eq (car *refusal*) :mutated-native-validation)))
          (insist (and (eql 2 exit) (eq :invalid-input-or-io (getf record :reason)))
                  :original-condition-public-projection))
        (when (eq *case* :declarations-only)
          (insist (null (getf (getf record :counts) :captured)) :declarations-not-captures)))))

(defun run-case (case backend evidence-root &key explicit cache-root)
  "Run ONE admitted control, not a native acceptance test. Returns public record.
EVIDENCE-ROOT must be absent, absolute and under the owned workspace. CACHE-ROOT
when supplied is a previously owned directory made by this program. Four required
positive invocations are CASE=:POSITIVE x BACKEND=x11/wayland x EXPLICIT=NIL/T.
All manifest-mutating routing negatives use an external explicit manifest.
Caller must authorize commands and view every unique PNG, including failures."
  (insist (member case +cases+) :known-control)
  (insist (member backend '("x11" "wayland") :test #'equal) :known-backend)
  (insist (and (char= (char evidence-root 0) #\/) (not (probe-file evidence-root))) :new-evidence-root)
  (let* ((*case* case) (*evidence* (directory! evidence-root))
         (*cache* (if cache-root (namestring (truename cache-root))
                      (directory! (cat *evidence* "reference-cache"))))
         (*calls* 0) (*comparisons* (make-array 6 :initial-element 0))
         (*step* :public-entry) (*invocation-root* nil)
         (*refusal* nil) (*completed* nil)
         (external (or explicit (routing-case-p case)))
         (manifest-path (when external (cat *evidence* "EXTERNAL-MANIFEST.sexp")))
         (args (append (list "--area" "double-size-characters" "--backend" backend)
                       (when external (list "--manifest" manifest-path))))
         (saved (symbol-function 'vttest-double-size-driver:invoke-native))
         (stdout (make-string-output-stream)) (stderr (make-string-output-stream))
         (exit nil) (text nil) (record nil))
    (when external
      (data manifest-path (mutate-manifest (vttest-double-size-driver:make-manifest backend)))
      ;; Different legal whitespace tests exact-octet staging, not reprinting.
      (append-text manifest-path (format nil "~%  ~%")))
    (data (cat *evidence* "PUBLIC-ARGS.sexp") args)
    (unwind-protect
         (progn
           (setf (symbol-function 'vttest-double-size-driver:invoke-native) #'native-fixture)
           (setf exit
             (with-environment
                 (list (cons "LISPTERM_VTTEST_EXACT_LABELS"
                             (if (eq case :wrong-env-label) "screen" "double-size"))
                       (cons "LISPTERM_VTTEST_FILTER" (when (eq case :wrong-env-filter) "screen")))
               (lambda () (let ((*standard-output* stdout) (*error-output* stderr))
                            (vttest-area:main args))))))
      (setf (symbol-function 'vttest-double-size-driver:invoke-native) saved)
      (setf text (get-output-stream-string stdout))
      (with-open-file (s (cat *evidence* "public.stdout") :direction :output :if-exists :error)
        (write-string text s))
      (with-open-file (s (cat *evidence* "public.stderr") :direction :output :if-exists :error)
        (write-string (get-output-stream-string stderr) s))
      (data (cat *evidence* "public.exit") exit)
      (data (cat *evidence* "CONTROL-PROGRESS.sexp")
        (list :case case :backend backend :explicit external :native-calls *calls*
              :compare-capture-calls (coerce *comparisons* 'list) :last-step *step*
              :completed-boundaries (reverse *completed*) :original-refusal (refusal-facts)
              :native-acceptance nil)))
    (setf record (read-data (public-result-path text)))
    (data (cat *evidence* "PUBLIC-RESULT-ORIGIN.sexp")
      (list :path (public-result-path text) :sha256 (digest (public-result-path text))))
    (assert-public record exit external)
    (data (cat *evidence* "CONTROL.sexp")
      (list :case case :backend backend :explicit external :assertions :pass
            :basis :synthetic-public-plumbing :native-acceptance nil))
    record))
