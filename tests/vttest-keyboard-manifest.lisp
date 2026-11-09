;;;; Closed keyboard manifests. Phases follow the gate's reviewed recorder:
;;;; 5.4 cursor keys RESET/SET/VT52 (3 x Up,Down,Left,Right then TAB),
;;;; 5.5 keypad ANSI numeric/application, VT52 numeric/application
;;;; (4 x KP_1,KP_2,KP_Enter then TAB), then one final Push <RETURN> hold.
;;;; Expected bytes are vttest-check:expected-keys, not a new encoder.
(in-package #:vttest-keyboard-driver)

(defparameter +archive+
  ".build/evidence/cursor-x11-first-screen-20261106-a/vttest-20251205.tgz")
(defparameter +archive-sha256+
  "cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc")
;; Byte reference: expected-keys / expected-key-byte-list and check-events.
(defparameter +oracle+ "tests/vttest-protocol.lisp")
;; Every project source loaded by tests/vttest-keyboard-native.sh, plus the shell
;; entrypoints. The closure follows the literal loads of vttest-area.lisp (all area
;; drivers), vttest-keyboard-launch.lisp and vttest-keyboard-native-main.lisp, which
;; loads vttest-keyboard-native-evidence.lisp and its prerequisite, font, decoder,
;; PNG and pixel files. vttest-gate.lisp is a recorder pin outside that closure.
(defparameter +source-paths+
  '("tests/vttest-area.lisp" "tests/vttest-selection.lisp" "tests/vttest-protocol.lisp"
    "tests/vttest-gate.lisp" "tests/vttest-cursor-driver.lisp"
    "tests/vttest-keyboard-driver.lisp" "tests/vttest-keyboard-manifest.lisp"
    "tests/vttest-keyboard-native-contract.lisp" "tests/vttest-keyboard-artifacts.lisp"
    "tests/vttest-keyboard-timing.lisp"
    "tests/vttest-keyboard-prefix.lisp" "tests/vttest-keyboard-child.lisp"
    "tests/vttest-keyboard-native.sh" "tests/vttest-keyboard-launch.lisp"
    "tests/vttest-keyboard-native-main.lisp" "tests/vttest-keyboard-cleanup.lisp"
    "tests/vttest-keyboard-cleanup-schema.lisp" "tests/vttest-keyboard-cleanup-status.lisp"
    "tests/vttest-keyboard-native-summary.lisp"
    "tests/vttest-gate.sh" "tests/wayland-gate.sh" "scripts/sbcl"
    ;; Area closure: cursor oracle and the other area drivers.
    "tests/vttest-cursor-oracle.lisp"
    "tests/vttest-screen-driver.lisp" "tests/vttest-screen-manifest.lisp"
    "tests/vttest-screen-native-contract.lisp" "tests/vttest-screen-frame.lisp"
    "tests/vttest-charsets-driver.lisp" "tests/vttest-charsets-native-contract.lisp"
    "tests/vttest-charsets-manifest.lisp"
    "tests/vttest-double-size-driver.lisp" "tests/vttest-double-size-native-contract.lisp"
    "tests/vttest-double-size-manifest.lisp"
    ;; Native evidence closure, in load order.
    "tests/vttest-keyboard-native-evidence.lisp" "tests/vttest-keyboard-native-prereq.lisp"
    "tests/vttest-keyboard-da.lisp" "tests/vttest-keyboard-prerequisites.lisp"
    "tests/vttest-keyboard-context.lisp" "tests/vttest-keyboard-row-output.lisp"
    "tests/vttest-keyboard-cells.lisp" "tests/vttest-screen-oracle.lisp"
    "tests/vttest-keyboard-context-font.lisp" "tests/vttest-keyboard-font-evidence.lisp"
    "tests/vttest-keyboard-sha256.lisp" "tests/vttest-keyboard-decoder.lisp"
    "src/display/package.lisp" "src/display/font-otb.lisp"
    "tests/vttest-keyboard-decoder-pin.lisp"
    "tests/vttest-keyboard-png.lisp" "tests/vttest-keyboard-pixels.lisp"))

(defun scenarios ()
  (loop for (label path) in +cases+ collect
    (list :id label :recorder-label label :menu-path path :pauses 1
          :captures (loop for id in (capture-ids label)
                          collect (list :id id :pause (if (equal id "pause01") 1 0)))
          ;; Timing is the event-order rule of check-events (strict :seq, monotonic
          ;; :time on one process clock, bounded replies, nonoverlapping native key
          ;; brackets, exact bytes) plus capture-errors frame proof for each original
          ;; capture, taken after its own key-end and before the next key-begin
          ;; (vttest-keyboard-timing.lisp). It is not a wall-clock latency bound and
          ;; not pixel acceptance; absent evidence is incomplete.
          :bytes t :timing t
          :oracle (list :path +oracle+ :sha256 (digest +oracle+))
          :requires nil)))

(defun make-manifest (backend)
  "Declare both keyboard cases and 26 original captures. Not a native observation."
  (need (member backend '("x11" "wayland") :test #'equal) :unknown-backend)
  (list :kind :vttest-area-manifest :version 1 :area "keyboard"
        :backend backend :scope-status :frozen
        :conditions '("private-headless" "source-built-per-run" "grid-80x24"
                      "exact-labels-keyboard-cursor-keyboard-keypad" "menu-5-4-and-5-5"
                      "native-key-brackets-15-and-16" "per-key-exact-bytes"
                      "final-hold-captured" "post-close-owned-cleanup")
        :scenarios (scenarios) :required-count 26
        :native-driver (format nil "keyboard-~a-v1" backend)
        :pins (cons (list :path +archive+ :sha256 +archive-sha256+)
                    (loop for path in +source-paths+
                          collect (list :path path :sha256 (digest path))))))

(defun validate (manifest)
  "Rehash declared bytes and require the exact closed declaration."
  (need (equal (digest +archive+) +archive-sha256+) :keyboard-archive-mismatch)
  (need (equal manifest (make-manifest (getf manifest :backend))) :keyboard-manifest-mismatch)
  manifest)
