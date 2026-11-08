;;;; Inert offline launcher controls. Invoke NATIVE-LAUNCH-CONTROLS explicitly.
(in-package #:vttest-keyboard-driver-tests)

(defun native-stub-text (variant)
 "Static mock protocol, not a selectable production gate."
 (concatenate 'string
  "#!/usr/bin/env bash
set -euo pipefail
[[ $# == 1 && $1 == x11 ]]
[[ $LISPTERM_VTTEST_MODE == record && $LISPTERM_VTTEST_SCALE == 1 ]]
[[ $LISPTERM_VTTEST_EXACT_LABELS == keyboard-cursor,keyboard-keypad ]]
[[ -z ${DISPLAY+x} && -z ${WAYLAND_DISPLAY+x} && -z ${SWAYSOCK+x} ]]
[[ -z ${LISPTERM_VTTEST_FILTER+x} && -z ${LISPTERM_VTTEST_ON_PHASE+x} ]]
[[ $LISPTERM_FONT == /mock/font && $VTTEST == /mock/vttest && $LISPTERM_SBCL == /mock/sbcl ]]
request=$LISPTERM_VTTEST_AREA_REQUEST
root=${request%/*}
mkdir \"$root/mock-launch-once\"
[[ $LISPTERM_VTTEST_ARTIFACT_ROOT == \"$root/recorder\" ]]
[[ $LISPTERM_VTTEST_AREA_RECEIPT == \"$root/launch-receipt.txt\" ]]
mkdir -p \"$root/recorder\"
cp -- \"$request\" \"$root/recorder/area-request.sexp\"
printf '%s\\n' \"$root/recorder\" > \"$root/launch-receipt.txt\"
printf 'mock-stdout\\n'
printf 'mock-stderr\\n' >&2
printf '88\\n' > \"$root/recorder/harness.exit\"
stat=$(cat /proc/$$/stat); stat=${stat##*) }; read -ra fields <<< \"$stat\"; start=${fields[19]}
hash=$(sha256sum \"$request\"); hash=${hash%% *}
title=Lispterm-vttest-0123456789abcdef-$$
trap 'exit 0' TERM
"
  (if (eq variant :startup-hang)
    "while :; do :; done
" "")
  (if (eq variant :missed-title)
    "printf 'closed' > \"$root/recorder/cleanup-receipt.sexp\"
" "")
  "printf 'title=%s\\n' \"$title\" > \"$root/recorder/fixture.txt\"

"
  (if (member variant '(:hang :title-error))
    "while :; do :; done
" "sleep 0.15
")
  (case variant
   (:missing-receipt "")
   (:malformed-receipt "printf '(' > \"$root/recorder/cleanup-receipt.sexp\"
")
   (otherwise
   "cat > \"$root/recorder/cleanup-receipt.sexp\" <<EOF
(:kind :vttest-gate-cleanup :version 1 :phase :post-close
 :launch (:gate-pid $$ :gate-start \"$start\" :title \"$title\" :artifacts \"$root/recorder\" :area-request-sha256 \"$hash\")
 :state :stopped :stop-owned :all-stopped :runtime :none
 :owned ((:role \"server\" :pid 1000001 :start \"1\" :state :stopped :observed :absent :direct t :wait-status 143)
     (:role \"client\" :pid 1000002 :start \"2\" :state :stopped :observed :absent :direct t :wait-status 0))
 :wayland-holder :not-applicable :launch-valid t :adoption-failures nil :identity-failures nil)
EOF
"))
  (case variant
   (:wrong-copy "printf x >> \"$root/recorder/area-request.sexp\"
")
   (:missing-link "rm -- \"$root/launch-receipt.txt\"
")
   (otherwise ""))
  (if (eq variant :gate-nonzero) "exit 7
" "exit 0
")))

(defun call-with-native-poison (thunk)
 (let* ((keys '("DISPLAY" "WAYLAND_DISPLAY" "SWAYSOCK"
         "LISPTERM_VTTEST_FILTER" "LISPTERM_VTTEST_ON_PHASE"))
     (old (mapcar (lambda (key) (cons key (sb-ext:posix-getenv key))) keys)))
  (unwind-protect
     (progn (dolist (key keys) (sb-posix:setenv key "mock-inherited-poison" 1))
        (funcall thunk))
   (dolist (pair old)
    (if (cdr pair) (sb-posix:setenv (car pair) (cdr pair) 1)
      (sb-posix:unsetenv (car pair)))))))

(defun native-launch-controls ()
 "Invoke only after driver-test helpers and shared consumer are loaded."
 (dolist (variant '(:complete :missed-title :missing-receipt :malformed-receipt
           :wrong-copy :missing-link :gate-nonzero :startup-hang :hang :title-error))
  (let* ((root (recorder)) (manifest (c::cat root "MANIFEST.sexp"))
      (stub (c::cat root "mock-keyboard-gate.sh"))
      (k::*keyboard-test-gate* stub) (m (k:make-manifest "x11"))
      (k::*keyboard-startup-seconds* 0.5) (k::*keyboard-completion-seconds* 1)
      (k::*keyboard-term-seconds* 0.5) (k::*keyboard-kill-seconds* 0.5)
      (old-wait (symbol-function 'k::keyboard-first-wait))
      (old-text (symbol-function 'k::native-text)) (joins 0)
      (began (get-internal-real-time)))
   (k::native-put manifest m)
   (with-open-file (s stub :direction :output :if-exists :error)
    (write-string (native-stub-text variant) s))
   (with-functions
    (list (cons 'k::native-runtime (lambda () (values "/mock/font" "/mock/vttest" "/mock/sbcl")))
       (cons 'k::keyboard-first-wait
          (lambda (child) (incf joins) (funcall old-wait child)))
       (cons 'k::native-text
          (lambda (path)
           (if (and (eq variant :title-error) (search "fixture.txt" path))
             (error "controlled title read error") (funcall old-text path)))))
    (lambda ()
     (let* ((code (call-with-native-poison
            (lambda () (k::native-main "x11" (subseq root 0 (1- (length root))) manifest))))
        (record (k::recorder-data (c::cat root "KEYBOARD-RUN.sexp")))
        (command (k::recorder-data (c::cat root "GATE-COMMAND.sexp")))
        (joined (k::recorder-data (c::cat root "GATE-EXIT.sexp")))
        (env (getf command :environment)))
      (check (= code (if (member variant '(:malformed-receipt :wrong-copy :gate-nonzero :title-error)) 1 3))
         (list :native-stub-classification variant))
      (check (and (= joins 1)
            (< (/ (- (get-internal-real-time) began) internal-time-units-per-second) 6))
         (list :bounded-first-terminal-join variant))
      (when (member variant '(:startup-hang :hang :title-error))
       (check (getf (k::recorder-data (c::cat root "NATIVE-OBSERVATIONS.sexp")) :errors)
           :interrupted-gate-retains-diagnostic))
      (check (probe-file (c::cat root "mock-launch-once/")) :exactly-one-mock-launch)
      (check (not (eq :pass (getf record :status))) :no-fabricated-pixel-timing-pass)
      (check (and (equal (getf command :arguments)
               (list "--noprofile" "--norc" stub "x11"))
            (equal env (k::keyboard-gate-environment "x11" root
                  (c::cat root "launch-request.sexp")
                  "/mock/font" "/mock/vttest" "/mock/sbcl")))
         :exact-private-argv-environment)
      (check (and (eql (getf joined :exit-code) (if (eq variant :gate-nonzero) 7 0))
            (getf joined :wait-observed)) :actual-join-not-forged-harness-exit)
      (check (equal (k::native-text (c::cat root "gate.stdout")) (format nil "mock-stdout~%"))
         :separate-gate-stdout)
      (check (equal (k::native-text (c::cat root "gate.stderr")) (format nil "mock-stderr~%"))
         :separate-gate-stderr)
      (check (eql (k::present-p (c::cat root "GATE-LAUNCH.sexp"))
            (not (member variant '(:missed-title :startup-hang :title-error)))) :preclose-title-only)
      (let ((request (k::recorder-data (c::cat root "launch-request.sexp"))))
       (check (equal request
              (list :kind :vttest-keyboard-launch :version 1 :backend "x11"
                 :nonce root :manifest-sha256 (k::digest manifest)
                 :exact-labels '("keyboard-cursor" "keyboard-keypad")))
          :closed-original-manifest-request))
      (when (eq variant :complete)
       (check (eq :pass (getf (k::recorder-data
                   (c::cat root "NATIVE-OBSERVATIONS.sexp")) :cleanup))
          :launch-bound-cleanup-baseline)
       (check (and (= 26 (length (getf record :checks)))
             (every (lambda (case) (eq :pass (k::case-identity (first case)
                            (c::cat root "recorder/")))) k::+cases+))
          :complete-original-identity-still-no-pixel-proof)
       (check (every (lambda (case) (eq :pass (k::case-bytes (first case)
                            (c::cat root "recorder/")))) k::+cases+)
          :complete-mechanical-bytes-still-incomplete)
       (check (= code 3) :complete-mechanical-no-visual-or-timing-proof)))))
   (check (and (eq old-wait (symbol-function 'k::keyboard-first-wait))
         (eq old-text (symbol-function 'k::native-text))) :lifecycle-mocks-restored)))
 (let ((root (temp-root)) (launches 0))
  (with-functions
   (list (cons 'k::launch-keyboard-gate
         (lambda (&rest args) (declare (ignore args)) (incf launches))))
   (lambda ()
    (check (= 2 (k::native-main "x11" root (c::cat root "MANIFEST.sexp")))
       :trailing-slash-run-rejected-before-launch)
    (check (zerop launches) :invalid-run-zero-launches)))))
