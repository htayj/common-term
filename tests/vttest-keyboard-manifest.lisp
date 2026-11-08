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
;; Static declarations of the present load chain; not a resolved execution closure.
(defparameter +source-paths+
  '("tests/vttest-area.lisp" "tests/vttest-selection.lisp" "tests/vttest-protocol.lisp"
    "tests/vttest-gate.lisp" "tests/vttest-cursor-driver.lisp"
    "tests/vttest-keyboard-driver.lisp" "tests/vttest-keyboard-manifest.lisp"
    "tests/vttest-keyboard-native-contract.lisp" "tests/vttest-keyboard-artifacts.lisp"
    "tests/vttest-keyboard-prefix.lisp" "tests/vttest-keyboard-child.lisp"
    "tests/vttest-keyboard-native.sh" "tests/vttest-keyboard-launch.lisp"
    "tests/vttest-keyboard-native-main.lisp" "tests/vttest-keyboard-cleanup.lisp"
    "tests/vttest-keyboard-cleanup-schema.lisp" "tests/vttest-keyboard-cleanup-status.lisp"
    "tests/vttest-keyboard-native-summary.lisp"
    "tests/vttest-gate.sh" "tests/wayland-gate.sh" "scripts/sbcl"))

(defun scenarios ()
  (loop for (label path) in +cases+ collect
    (list :id label :recorder-label label :menu-path path :pauses 1
          :captures (loop for id in (capture-ids label)
                          collect (list :id id :pause (if (equal id "pause01") 1 0)))
          ;; No timing oracle is implemented; absent timing evidence is incomplete.
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
