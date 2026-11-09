;;;; Standalone runner, without project bootstrap or native dependencies.
(load "tests/vttest-area.lisp")

(defun run-screen-controls ()
  "Focused pure/mock controls. Never invoke a build or native session."
  (let ((checks 0))
    (labels ((ok (value) (assert value) (incf checks))
             (refused (thunk)
               (handler-case (progn (funcall thunk) nil)
                 (vttest-area:gate-problem () t)))
             ;; Mock-only version-3 receipt with placeholder references. It proves
             ;; protocol shape against the unrooted validator, never pixels or timing.
             (native-record (backend &optional (status :pass))
               (let ((origin (list :path "recorder/RESULT.sexp" :sha256 (make-string 64 :initial-element #\a))))
                 (list :kind :vttest-screen-run :version 3 :backend backend
                       :attempted 15 :reached 15 :captured 15
                       :checks (loop for n from 1 to 15 collect
                                 (list :pause n :pixels :pass :bytes :pass :timing :pass))
                       :status status :build :pass :native-started t :cleanup :pass
                       :recorder-result "recorder/RESULT.sexp" :recorder-sha256 (make-string 64 :initial-element #\a)
                       :manifest "MANIFEST.sexp" :manifest-sha256 (make-string 64 :initial-element #\b)
                       :execution-inputs "native/execution-inputs.sha256"
                       :evidence (list "native/" "build/BUILD.sexp" "recorder/RESULT.sexp")
                       :reason "mock-only protocol record; no native pixels or timing observed"
                       :stages (list :recorder (list :origin origin :attempted 15 :reached 15 :captured 15 :frame-verified 15)
                                     :native (list :origin (list :path "native/stages.sexp"
                                                                 :sha256 (make-string 64 :initial-element #\c))
                                                   :request-observed 15 :capture-invoked 15 :capture-completed 15))
                       :finalization (list :state :complete :client-exit (if (eq status :pass) 0 3)
                                           :result (copy-tree origin) :abort nil)
                       :abort nil :unowned-endpoints nil :count-basis :native-legacy)))
             (refused-receipt (manifest record)
               ;; The summary converts contract refusals into RECEIPT-ERROR.
               (multiple-value-bind (observations evidence)
                   (vttest-screen-driver:summarize-native manifest record 0)
                 (ok (stringp (getf evidence :receipt-error)))
                 (ok (null (getf evidence :native-record)))
                 (ok (= (length observations) 15))
                 (ok (every (lambda (o) (and (eq (getf o :pixels) :unverified) (eq (getf o :timing) :unverified)
                                             (not (getf o :fresh))))
                            observations))
                 (ok (= (getf (getf evidence :counts) :unverified) 15))
                 (ok (eq (vttest-area:aggregate-observations (getf manifest :scenarios) observations) :unverified))
                 (ok (getf evidence :native-failed))))
             (fixture-put (root path value)
               (vttest-screen-native-contract:write-data (concatenate 'string root path) value))
             (fixture-ref (root path) (vttest-screen-native-contract:reference root path))
             (fixture-text (root path text)
               (with-open-file (s (concatenate 'string root path) :direction :output
                                  :if-exists :error :if-does-not-exist :create)
                 (write-string text s)))
             (fixture-status (bk n frames)
               (let ((cols (if (member n '(3 5)) 132 80)))
                 (list :id 1 :child-pid 2 :backend bk :frames frames :bytes-received (+ 100 n)
                       :rows 24 :columns cols :pty-rows 24 :pty-columns cols
                       :blink-phase (= n 14) :blink-visible (>= n 13) :input-pending 0 :error-count 0
                       :process-state :running :transport-state :open
                       :geometry (append (list :phase :settled :request-id 1 :display-epoch 1 :epoch 1
                                               :actual-width (* cols 8) :actual-height 384
                                               :cell-width 8 :cell-height 16 :presentation 2
                                               :target-presentation 2 :rows 24 :columns cols :window-id 7)
                                         (when (eq bk :wayland)
                                           (list :frame-pending-p nil :commit-generation 1 :frame-generation 1))))))
             (rooted-record (root backend exit)
               ;; Mock-only invocation tree for real rooted validation. Exit 0 gets a
               ;; complete fabricated PASS tree with placeholder PNG and zero RGBA
               ;; bytes; exits 1 and 3 get valid records that never started native.
               (let ((bk (intern (string-upcase backend) :keyword)) (checks nil) (entries nil)
                     (decoder "/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick"))
                 (ensure-directories-exist (concatenate 'string root "recorder/"))
                 (ensure-directories-exist (concatenate 'string root "native/"))
                 (when (zerop exit)
                   (loop for n from 1 to 15 do
                     (let* ((stem (format nil "recorder/pause-~2,'0d-" n))
                            (width (if (member n '(3 5)) 1056 640))
                            (ack (if (eq bk :x11) -1 (+ 100 n)))
                            (native (list :version 3 :backend bk :pause n :window-id 7 :native-window-id 7
                                          :surface-id 9 :width width :height 384 :scale 1 :capture-count 1
                                          :owned t :capture-exit 0 :png (format nil "pause-~2,'0d-actual.png" n)
                                          :ack-output ack :ack-basis :recorder-request-binding :custody t))
                            (png (concatenate 'string root stem "actual.png"))
                            (rgba (concatenate 'string root stem "actual.rgba"))
                            (commands nil))
                       (loop for suffix in '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp")
                             for frames in '(1 2 2 2)
                             do (fixture-put root (concatenate 'string stem suffix) (fixture-status bk n frames)))
                       (fixture-put root (concatenate 'string stem "native.sexp") native)
                       (fixture-put root (concatenate 'string stem "request.sexp")
                                    (list :status (fixture-status bk n 2) :pause n :target (+ 100 n)
                                          :ack-output ack :surface-id 9))
                       (fixture-text root (concatenate 'string stem "request") "Mock-only request binding.")
                       (fixture-put root (concatenate 'string stem "capture.exit") 0)
                       (fixture-text root (concatenate 'string stem "actual.png") "Mock-only placeholder, not a capture.")
                       (with-open-file (s rgba :direction :output :if-exists :error :if-does-not-exist :create
                                          :element-type '(unsigned-byte 8))
                         (write-sequence (make-array (* width 384 4) :element-type '(unsigned-byte 8)
                                                                     :initial-element 0) s))
                       (loop for action in '("identify" "decode")
                             for argv in (list (list decoder "identify" "-format" "%w %h" png)
                                               (list decoder png "-depth" "8" (concatenate 'string "rgba:" rgba)))
                             for base = (concatenate 'string stem action)
                             do (fixture-put root (concatenate 'string base ".argv.sexp") argv)
                                (fixture-text root (concatenate 'string base ".stdout") "")
                                (fixture-text root (concatenate 'string base ".stderr") "")
                                (fixture-put root (concatenate 'string base ".exit") 0)
                                (push (loop for suffix in '(".argv.sexp" ".stdout" ".stderr" ".exit")
                                            collect (fixture-ref root (concatenate 'string base suffix)))
                                      commands))
                       (fixture-put root (concatenate 'string stem "comparison.sexp")
                                    (list :version 1 :pause n :png (fixture-ref root (concatenate 'string stem "actual.png"))
                                          :phase (= n 14) :width width :height 384
                                          :font (list :path "/nix/store/9qvd27vg12177prxcl778rdhxh4lk2mi-terminus-font-4.49.1/share/fonts/terminus/ter-u16n.otb"
                                                      :sha256 "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
                                          :decoder (list :path decoder
                                                         :sha256 "054c52b583c303bca2aae9fadcfd9e09fbc946f83e6621b39f22650d28d3027e")
                                          :bracket (loop for suffix in '("before.sexp" "ready.sexp" "pre.sexp" "final.sexp" "native.sexp")
                                                         collect (fixture-ref root (concatenate 'string stem suffix)))
                                          :identify (second commands) :decode (first commands)
                                          :rgba (fixture-ref root (concatenate 'string stem "actual.rgba"))
                                          :decoded-length (* width 384 4) :opaque t
                                          :comparison (list :pass t :compared-pixels (* width 384) :different-pixels 0
                                                            :different-cells nil :first-difference nil :tolerance 0
                                                            :masked-pixels 0)))
                       (push (list :pause n :pixels :pass :bytes :unverified :timing :pass :frame :pass
                                   :target :recognized-navigation-only :native native
                                   :comparison (fixture-ref root (concatenate 'string stem "comparison.sexp")))
                             checks)
                       (push (list :pause n :request (fixture-ref root (concatenate 'string stem "request"))
                                   :invoked t :completed t :capture-exit 0
                                   :exit-origin (fixture-ref root (concatenate 'string stem "capture.exit"))
                                   :receipt (fixture-ref root (concatenate 'string stem "native.sexp")))
                             entries)))
                   (setf checks (nreverse checks) entries (nreverse entries))
                   (fixture-put root "recorder/RESULT.sexp"
                                (list :kind :vttest-screen-run :version 2 :backend backend :status :pass
                                      :attempted 15 :reached 15 :captured 15 :frame-verified 15
                                      :bytes :unverified :wire-accounting :unverified
                                      :reason "mock-only fixture; no native session" :cleanup :graceful-exit
                                      :checks checks
                                      :evidence (list "events.sexp" "output.raw" "input.raw" "application.log"
                                                      "launch.sexp" "terminal-status.sexp")))
                   (fixture-put root "native/client.exit" 0)
                   (fixture-put root "native/finalization-clock.sexp"
                                (list :clock :clock-monotonic :limit-seconds 15 :start-ns 100 :end-ns 200
                                      :state :exited :pid 1 :token "mock-only-not-a-process"
                                      :run root :backend backend))
                   (fixture-put root "native/import-deadline.sexp"
                                (list :clock :clock-monotonic :deadline-ns 15000000100 :validated-ns 300 :state :valid)))
                 (fixture-put root "native/stages.sexp"
                              (list :kind :vttest-screen-native-stages :version 1 :backend backend :entries entries))
                 (let* ((pass (zerop exit)) (fail (eql exit 1)) (n (if pass 15 0))
                        (r (when pass (fixture-ref root "recorder/RESULT.sexp"))))
                   (list :kind :vttest-screen-run :version 3 :backend backend
                         :attempted n :reached n :captured n
                         :checks (loop for c in checks collect
                                   (loop for k in '(:pause :pixels :bytes :timing) append (list k (getf c k))))
                         :status (cond (pass :pass) (fail :fail) (t :unverified))
                         :build (cond (pass :pass) (fail :fail) (t :unverified))
                         :native-started pass :cleanup (if pass :pass :not-applicable)
                         :recorder-result "recorder/RESULT.sexp" :recorder-sha256 (getf r :sha256)
                         :manifest (concatenate 'string root "MANIFEST.sexp")
                         :manifest-sha256 (getf (fixture-ref root "MANIFEST.sexp") :sha256)
                         :execution-inputs "native/execution-inputs.sha256"
                         :evidence (list "native/" "build/BUILD.sexp" "recorder/RESULT.sexp")
                         :reason (cond (pass "mock-only rooted PASS fixture") (fail "mock-only build failure")
                                       (t "mock-only native not started"))
                         :stages (list :recorder (if pass
                                                     (list :origin r :attempted 15 :reached 15 :captured 15 :frame-verified 15)
                                                     (list :origin nil :attempted nil :reached nil :captured nil :frame-verified nil))
                                       :native (list :origin (fixture-ref root "native/stages.sexp")
                                                     :request-observed n :capture-invoked n :capture-completed n))
                         :finalization (list :state (if pass :complete :not-started) :client-exit (when pass 0)
                                             :result (copy-tree r) :abort nil)
                         :abort nil :unowned-endpoints nil :count-basis :native-legacy)))))
      (dolist (backend '("x11" "wayland"))
        (dolist (alias '("screen" "screen-features"))
          (multiple-value-bind (area selected path)
              (vttest-area::parse-cli (list "--area" alias "--backend" backend))
            (ok (equal area "screen")) (ok (equal selected backend)) (ok (null path))))
        (let ((manifest (vttest-screen-driver:make-manifest backend)))
          (ok (vttest-area:validate-manifest manifest "screen" backend))
          (ok (vttest-screen-driver:validate manifest))
          (dolist (mutation '( :backend :native-driver :pins :scenarios))
            (let ((copy (copy-tree manifest)))
              (setf (getf copy mutation) nil)
              (ok (refused (lambda () (vttest-screen-driver:validate copy))))))
          (ok (refused (lambda ()
                        (vttest-area:validate-manifest
                         (append (list :backend backend) manifest) "screen" backend))))
          (multiple-value-bind (observations evidence)
              (vttest-screen-driver:summarize-native manifest (native-record backend) 0)
            (ok (null (getf evidence :receipt-error)))
            (ok (getf evidence :native-record))
            (ok (= (length observations) 15))
            (ok (eq (vttest-area:aggregate-observations (getf manifest :scenarios) observations) :pass))
            (ok (= (getf (getf evidence :counts) :passed) 15)))
          (multiple-value-bind (observations evidence)
              (vttest-screen-driver:summarize-native manifest nil nil)
            (ok (eq (vttest-area:aggregate-observations (getf manifest :scenarios) observations) :unverified))
            (ok (null (getf (getf evidence :counts) :captured)))
            (ok (= (getf (getf evidence :counts) :unverified) 15)))
          (let ((record (native-record backend :unverified)))
            (setf (getf record :captured) 0 (getf record :checks) nil)
            (multiple-value-bind (observations evidence)
                (vttest-screen-driver:summarize-native manifest record 3)
              (declare (ignore observations))
              (ok (null (getf evidence :receipt-error)))
              (ok (eql (getf (getf evidence :counts) :captured) 0))
              (ok (not (getf evidence :native-failed)))))
          ;; A pixel failure imports as a :FAIL record, as the native publisher does.
          (let ((record (native-record backend :fail)))
            (setf (getf (first (getf record :checks)) :pixels) :fail)
            (multiple-value-bind (observations evidence)
                (vttest-screen-driver:summarize-native manifest record 3)
              (ok (null (getf evidence :receipt-error)))
              (ok (eq (vttest-area:aggregate-observations (getf manifest :scenarios) observations) :fail))
              (ok (= (getf (getf evidence :counts) :failed) 1))
              (ok (getf evidence :native-failed))))
          (dolist (bad '( :duplicate :backend :pause :status :count))
            (let ((record (native-record backend)))
              (case bad
                (:duplicate (push (copy-list (first (getf record :checks))) (getf record :checks)))
                (:backend (setf (getf record :backend) "other"))
                (:pause (setf (getf (first (getf record :checks)) :pause) 16))
                (:status (setf (getf (first (getf record :checks)) :bytes) :ready))
                (:count (setf (getf record :attempted) -1)))
              (refused-receipt manifest record)))))
      (dolist (args '(("--area" "screen" "--area" "screen")
                      ("--area" "screen" "--backend" "X11")
                      ("--area" "cursor-movements" "--backend" "x11")))
        (ok (refused (lambda () (vttest-area::parse-cli args)))))
      (multiple-value-bind (area backend path)
          (vttest-area::parse-cli '("--area" "cursor-movements" "--backend" "x11" "--manifest" "tests/fixtures/vttest-area/frozen.sexp"))
        (ok (equal area "cursor-movements")) (ok (equal backend "x11")) (ok path))
      ;; Scale selection: X11 only 1, Wayland 1 or 2; strict manifest :SCALE.
      (ok (eql 1 (nth-value 3 (vttest-area::parse-cli '("--area" "screen" "--backend" "x11")))))
      (ok (eql 2 (nth-value 3 (vttest-area::parse-cli '("--area" "screen" "--backend" "wayland" "--scale" "2")))))
      (ok (eql 1 (nth-value 3 (vttest-area::parse-cli '("--scale" "1" "--area" "keyboard" "--backend" "wayland")))))
      (dolist (args '(("--area" "screen" "--backend" "x11" "--scale" "2")
                      ("--area" "screen" "--backend" "wayland" "--scale" "3")
                      ("--area" "screen" "--backend" "wayland" "--scale" "0")
                      ("--area" "screen" "--backend" "wayland" "--scale" "02")
                      ("--area" "screen" "--backend" "wayland" "--scale" "2.0")
                      ("--area" "screen" "--backend" "wayland" "--scale")
                      ("--area" "screen" "--backend" "wayland" "--scale" "2" "--scale" "2")
                      ("--area" "screen" "--backend" "wayland" "--zoom" "2")))
        (ok (refused (lambda () (vttest-area::parse-cli args)))))
      ;; Eight arguments reach the flag check; ten are refused earlier as usage.
      (dolist (control '((("--area" "screen" "--backend" "wayland" "--scale" "1" "--scale" "2") :repeated-flag)
                         (("--area" "screen" "--backend" "wayland" "--scale" "2" "--scale" "2" "--scale" "2") :usage)))
        (handler-case (progn (vttest-area::parse-cli (first control)) (ok nil))
          (vttest-area:gate-problem (e) (ok (eq (vttest-area:problem-reason e) (second control))))))
      (ok (eql 2 (vttest-area::check-scale-environment 2 nil)))
      (ok (eql 2 (vttest-area::check-scale-environment 2 "2")))
      (dolist (inherited '("1" "" "2 " "02"))
        (ok (refused (lambda () (vttest-area::check-scale-environment 2 inherited)))))
      (ok (refused (lambda () (vttest-area::check-scale-environment 1 "2"))))
      (flet ((factories (backend scale)
               (list (list "screen" (vttest-screen-driver:make-manifest backend scale) #'vttest-screen-driver:validate)
                     (list "character-sets" (vttest-charsets-driver:make-manifest backend scale) #'vttest-charsets-driver:validate)
                     (list "double-size-characters" (vttest-double-size-driver:make-manifest backend scale)
                           #'vttest-double-size-driver:validate))))
        (dolist (selection '(("x11" 1) ("wayland" 1) ("wayland" 2)))
          (destructuring-bind (backend scale) selection
            (loop for (area m validate) in (factories backend scale) do
              (ok (eql scale (getf m :scale)))
              (ok (eq m (vttest-area:validate-manifest m area backend scale)))
              (ok (eq m (funcall validate m)))
              (ok (refused (lambda () (vttest-area:validate-manifest m area backend (- 3 scale)))))
              (let ((missing (copy-list m))) (remf missing :scale)
                (ok (refused (lambda () (vttest-area:validate-manifest missing area backend scale))))
                (ok (refused (lambda () (funcall validate missing)))))
              (ok (refused (lambda () (vttest-area:validate-manifest (append m (list :zoom 1)) area backend scale))))
              (ok (refused (lambda () (vttest-area:validate-manifest (list* :scale scale m) area backend scale))))
              (dolist (bad (list 3 0 "2" 2.0 nil))
                (let ((copy (copy-list m))) (setf (getf copy :scale) bad)
                  (ok (refused (lambda () (vttest-area:validate-manifest copy area backend scale))))
                  (ok (refused (lambda () (funcall validate copy)))))))))
        (dolist (make (list #'vttest-screen-driver:make-manifest #'vttest-charsets-driver:make-manifest
                            #'vttest-double-size-driver:make-manifest))
          (ok (refused (lambda () (funcall make "x11" 2))))
          (ok (refused (lambda () (funcall make "wayland" 3)))))
        (ok (equal "scale-2" (third (getf (vttest-double-size-driver:make-manifest "wayland" 2) :conditions)))))
      ;; Unscaled areas keep their scale-1 schema; scale 2 fails closed until integrated.
      (let ((m (vttest-keyboard-driver:make-manifest "wayland")))
        (ok (null (getf m :scale)))
        (ok (eq m (vttest-area:validate-manifest m "keyboard" "wayland")))
        (ok (refused (lambda () (vttest-area:validate-manifest m "keyboard" "wayland" 2))))
        (ok (refused (lambda () (vttest-area:validate-manifest (list* :scale 1 m) "keyboard" "wayland" 1)))))
      (handler-case
          (progn (vttest-area:validate-manifest '(:areas ((:area "screen-features")) :area-dependencies (t)) "screen" "x11")
                 (error "Documentary manifest accepted"))
        (vttest-area:gate-problem (e) (ok (= (vttest-area:problem-code e) 3))
                                    (ok (eq (vttest-area:problem-reason e) :scope-not-executable))))
      ;; Real public main and result persistence, with only the native boundary mocked.
      (let ((original (symbol-function 'vttest-screen-driver:invoke-native)))
        (unwind-protect
             (dolist (backend '("x11" "wayland"))
               (dolist (exit '(0 1 3))
                (setf (symbol-function 'vttest-screen-driver:invoke-native)
                      (lambda (selected root path)
                        (ok (equal selected backend)) (ok (probe-file path))
                        (fixture-put root "SCREEN-RUN.sexp" (rooted-record root backend exit))
                        exit))
                (let ((out (make-string-output-stream)) (err (make-string-output-stream)))
                  (let* ((*standard-output* out) (*error-output* err)
                         (actual (vttest-area:main (list "--area" "screen" "--backend" backend))))
                    (ok (= actual exit))
                    (let* ((text (get-output-stream-string out))
                           (start (+ 7 (search "RESULT " text)))
                           (end (position #\Newline text :start start))
                           (result (vttest-area:read-data (subseq text start end)))
                           (evidence (getf result :driver-evidence)))
                      (ok (= (getf result :exit) exit))
                      (ok (= (getf evidence :native-exit) exit))
                      (ok (= (getf (getf result :counts) :required) 15))
                      (ok (eq (getf result :outcome) (case exit (0 :pass) (1 :fail) (t :unverified))))
                      (ok (null (getf evidence :receipt-error)))
                      (ok (null (getf evidence :receipt-read-error)))
                      (ok (getf evidence :native-record))
                      (ok (if (eql exit 1) (getf evidence :native-failed) (null (getf evidence :native-failed))))
                      (ok (eql (getf (getf result :counts) :passed) (if (zerop exit) 15 0))))))))
          (setf (symbol-function 'vttest-screen-driver:invoke-native) original)))
      ;; Scale export lifetime: MAIN restores the prior LISPTERM_VTTEST_SCALE, or
      ;; unsets it, after normal, erroring and non-local native exits.
      (let ((original (symbol-function 'vttest-screen-driver:invoke-native))
            (saved (sb-ext:posix-getenv "LISPTERM_VTTEST_SCALE"))
            (invoked nil) (native-exit nil))
        (labels ((current () (sb-ext:posix-getenv "LISPTERM_VTTEST_SCALE"))
                 (set-scale (value)
                   (if value
                       (sb-posix:setenv "LISPTERM_VTTEST_SCALE" value 1)
                       (sb-posix:unsetenv "LISPTERM_VTTEST_SCALE")))
                 (args (backend scale)
                   (append (list "--area" "screen" "--backend" backend) (when scale (list "--scale" scale))))
                 (run-main (arguments)
                   (let* ((out (make-string-output-stream))
                          (exit (let ((*standard-output* out) (*error-output* (make-broadcast-stream)))
                                  (vttest-area:main arguments)))
                          (text (get-output-stream-string out))
                          (start (+ 7 (search "RESULT " text))))
                     (values exit (vttest-area:read-data (subseq text start (position #\Newline text :start start)))))))
          (unwind-protect
               (progn
                 (setf (symbol-function 'vttest-screen-driver:invoke-native)
                       (lambda (selected root path)
                         (declare (ignore root path))
                         (push (list selected (current)) invoked)
                         (funcall native-exit)))
                 ;; Normal native return, in sequence: a scale-2 run must not make a
                 ;; later scale-1 run look like an inherited mismatch.
                 (setf native-exit (lambda () 3))
                 (dolist (control '((nil "wayland" "2") (nil "wayland" "1") (nil "x11" nil)
                                    ("2" "wayland" "2") ("1" "wayland" "1") ("1" "x11" nil)))
                   (destructuring-bind (prior backend scale) control
                     (set-scale prior) (setf invoked nil)
                     (multiple-value-bind (exit result) (run-main (args backend scale))
                       (ok (equal invoked (list (list backend (or scale "1")))))
                       (ok (eql (getf result :scale) (if (equal scale "2") 2 1)))
                       (ok (not (eq (getf result :reason) :inherited-scale-mismatch)))
                       (ok (eql exit (getf result :exit))))
                     (ok (equal (current) prior))))
                 ;; Erroring native boundary.
                 (setf native-exit (lambda () (error "mock native failure")))
                 (dolist (prior '(nil "2"))
                   (set-scale prior) (setf invoked nil)
                   (run-main (args "wayland" "2"))
                   (ok (equal invoked '(("wayland" "2"))))
                   (ok (equal (current) prior)))
                 ;; Non-local exit straight through MAIN.
                 (setf native-exit (lambda () (throw 'escaped-main :escaped)))
                 (dolist (prior '(nil "2"))
                   (set-scale prior) (setf invoked nil)
                   (ok (eq :escaped (catch 'escaped-main
                                      (let ((*standard-output* (make-broadcast-stream))
                                            (*error-output* (make-broadcast-stream)))
                                        (vttest-area:main (args "wayland" "2"))))))
                   (ok (equal invoked '(("wayland" "2"))))
                   (ok (equal (current) prior)))
                 ;; An inherited mismatch is still refused before launching.
                 (setf native-exit (lambda () 3))
                 (dolist (control '(("1" "2") ("2" nil) ("" "1")))
                   (destructuring-bind (prior scale) control
                     (set-scale prior) (setf invoked nil)
                     (multiple-value-bind (exit result) (run-main (args "wayland" scale))
                       (ok (eql exit 2))
                       (ok (eq (getf result :reason) :inherited-scale-mismatch)))
                     (ok (null invoked))
                     (ok (equal (current) prior)))))
            (setf (symbol-function 'vttest-screen-driver:invoke-native) original)
            (set-scale saved))))
      (format t "SCREEN focused controls: ~d assertions PASS~%" checks) t)))

(if (equal (sb-ext:posix-getenv "LISPTERM_SCREEN_FOCUSED") "1")
    (sb-ext:exit :code (if (run-screen-controls) 0 1))
    (progn
      (load "tests/vttest-area-tests.lisp")
      (load "tests/vttest-cursor-driver-tests.lisp")
      (sb-ext:exit :code (if (and (run-screen-controls)
                                (funcall (find-symbol "RUN" "VTTEST-AREA-TESTS"))
                                (funcall (find-symbol "RUN" "VTTEST-CURSOR-DRIVER-TESTS"))) 0 1))))
