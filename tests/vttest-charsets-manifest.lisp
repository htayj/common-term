;;;; Complete character-set area declaration. This is not a native receipt.
(in-package #:vttest-charsets-driver)

(defparameter +charsets-archive+
  ".build/evidence/cursor-x11-first-screen-20261106-a/vttest-20251205.tgz")
(defparameter +charsets-source-sha+
  "cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc")
(defparameter +charsets-bash+
  "/nix/store/kwyi4bq5s2qgwbhz6cs4gjy57c2zdxws-bash-interactive-5.3p20/bin/bash")

;; These are source bindings, not evidence that a launch profile was reached.
;; Runtime/tool/library resolution and the canonical serialized manifest must
;; additionally be frozen by the native launch owner before backend execution.
(defparameter +charsets-pins+
  '("tests/vttest-area.lisp" "tests/vttest-charsets-driver.lisp"
    "tests/vttest-charsets-manifest.lisp" "tests/vttest-charsets-native.sh"
    "tests/vttest-charsets-recorder.lisp" "tests/vttest-charsets-native-contract.lisp"
    "tests/vttest-charsets-pixels.lisp" "tests/vttest-charsets-oracle.lisp"
    "tests/vttest-screen-build.sh" "tests/vttest-screen-frame.lisp"
    "tests/vttest-screen-oracle.lisp" "tests/vttest-cursor-oracle.lisp"
    "tests/vttest-cursor-driver.lisp" "tests/vttest-protocol.lisp"
    "tests/vttest-screen-driver.lisp" "tests/vttest-screen-manifest.lisp"
    "tests/vttest-screen-native-contract.lisp" "tests/vttest-screen-wire.lisp"
    "tests/vttest-selection.lisp" "tests/verify.sh"
    "scripts/sbcl" "scripts/deps.nix" "scripts/bootstrap.lisp" "lispterm.asd"
    "src/package.lisp" "src/conditions.lisp" "src/engine/compat.lisp"
    "engine/nerimux/src/package-types.lisp" "engine/nerimux/src/package.lisp"
    "engine/nerimux/src/cell.lisp" "engine/nerimux/src/screen-data.lisp"
    "engine/nerimux/src/screen.lisp" "engine/nerimux/src/screen-metadata.lisp"
    "engine/nerimux/src/screen-resize.lisp" "engine/nerimux/src/screen-logic.lisp"
    "engine/nerimux/src/scroll.lisp" "engine/nerimux/src/erase.lisp"
    "engine/nerimux/src/edit.lisp" "engine/nerimux/src/cursor.lisp"
    "engine/nerimux/src/char-write-definitions.lisp" "engine/nerimux/src/char-write-cells.lisp"
    "engine/nerimux/src/char-write.lisp" "engine/nerimux/src/modes-alt-screen.lisp"
    "engine/nerimux/src/modes-dec-pm-definitions.lisp" "engine/nerimux/src/modes-cursor-save.lisp"
    "engine/nerimux/src/modes-reset.lisp" "engine/nerimux/src/modes-charset-definitions.lisp"
    "engine/nerimux/src/modes-charset.lisp" "engine/nerimux/src/modes-ansi-sm-rm-definitions.lisp"
    "engine/nerimux/src/screen-projection.lisp" "engine/nerimux/src/screen-osc-state.lisp"
    "engine/nerimux/src/sgr-definitions.lisp" "engine/nerimux/src/sgr-colors.lisp"
    "engine/nerimux/src/sgr.lisp" "engine/nerimux/src/sgr-report.lisp"
    "engine/nerimux/src/csi-replies-definitions.lisp" "engine/nerimux/src/csi-replies.lisp"
    "engine/nerimux/src/csi-parameters.lisp" "engine/nerimux/src/csi-dispatch.lisp"
    "engine/nerimux/src/csi.lisp" "engine/nerimux/src/csi-device-rules.lisp"
    "engine/nerimux/src/csi-extended-rules.lisp" "engine/nerimux/src/csi-compose.lisp"
    "engine/nerimux/src/parser-dcs.lisp" "engine/nerimux/src/parser-core.lisp"
    "engine/nerimux/src/parser-csi.lisp" "engine/nerimux/src/parser-utf8.lisp"
    "engine/nerimux/src/parser.lisp" "engine/nerimux/src/parser-vt52.lisp"
    "engine/nerimux/src/parser-osc-clipboard.lisp" "engine/nerimux/src/parser-osc-uri.lisp"
    "engine/nerimux/src/parser-osc-color.lisp" "engine/nerimux/src/parser-osc-dispatch.lisp"
    "engine/nerimux/src/parser-osc.lisp" "engine/nerimux/src/emulator.lisp"
    "src/engine/adapter.lisp" "src/session.lisp" "src/renderer.lisp"
    "src/window.lisp" "src/host.lisp"
    "src/pty/package.lisp" "src/pty/ffi.lisp" "src/pty/posix.lisp" "src/pty/process.lisp"
    "src/display/package.lisp" "src/display/cells.lisp" "src/display/font-otb.lisp"
    "src/display/font.lisp" "src/display/keys.lisp" "src/display/protocol.lisp"
    "src/display/framebuffer.lisp" "src/display/x11.lisp" "src/display/wayland-wire.lisp"
    "src/display/wayland-protocol.lisp" "src/display/wayland-shm.lisp"
    "src/display/xkb.lisp" "src/display/wayland.lisp"))

(defun charsets-digest (path)
  (vttest-area::sha256 (vttest-cursor-driver::bytes path)))

(defun charsets-scenarios ()
  (list (list :id "charsets-01" :recorder-label "charsets"
              :menu-path '(3) :pauses 1
              :captures (list (list :id "pause-01" :pause 1))
              :bytes nil :timing t
              :oracle (list :path "tests/vttest-charsets-oracle.lisp"
                            :sha256 (charsets-digest "tests/vttest-charsets-oracle.lisp"))
              :requires nil)))

(defun make-manifest (backend)
  "Declare one complete reference capture; no launch readiness is asserted.
Both bare SCS 1/2 select ASCII under the matches-xterm emulator policy.
The actual stable boolean blink phase, including NIL, is used unchanged."
  (vttest-area::need (member backend '("x11" "wayland") :test #'equal) :unknown-backend)
  (list :kind :vttest-area-manifest :version 1 :area "character-sets" :backend backend
        :scope-status :frozen
        :conditions '("private-headless" "source-built-per-run"
          "complete-character-set-area" "matches-xterm" "bare-scs-1-2-ascii"
          "both-g0-si-and-g1-so" "ansi-vt100-seven-bit-gl" "grid-80x24-cell-8x16"
          "single-lines-full-margins-absolute-origin" "normal-screen-sgr-highlight-off"
          "strict-original-frame-bracket" "actual-stable-boolean-blink-phase"
          "no-text-blink-obligation" "exact-rgba-all-pixels" "fresh-native-custody")
        :scenarios (charsets-scenarios) :required-count 1
        :native-driver (format nil "charsets-~a-v1" backend)
        :pins (cons (list :path +charsets-archive+ :sha256 +charsets-source-sha+)
                    (loop for path in +charsets-pins+
                          collect (list :path path :sha256 (charsets-digest path))))))

(defun validate (manifest)
  "Require the exact current declaration, including original archive bytes."
  (vttest-area::need (equal (charsets-digest +charsets-archive+) +charsets-source-sha+)
                    :charsets-source-archive-mismatch)
  (vttest-area::need (equal manifest (make-manifest (getf manifest :backend)))
                    :charsets-manifest-mismatch)
  manifest)
