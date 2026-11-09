;;;; Six independent vttest menu-4 references. Loaded by the area driver.
(in-package #:vttest-double-size-driver)

(defparameter +archive+
  ".build/evidence/cursor-x11-first-screen-20261106-a/vttest-20251205.tgz")
(defparameter +archive-sha256+
  "cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc")
(defparameter +oracle+ "tests/vttest-double-size-oracle.lisp")
(defparameter +oracle-sha256+
  "9cf68971a097827d32b310622c976f6d8443b66d3be93e495796bdfa2864c750")
;; Static declarations cover this driver, the present dispatcher load chain and
;; the native adapter's named sources. They are NOT the resolved executed closure.
;; Native execution additionally requires its independently frozen
;; EXECUTION-INPUTS.sha256 (launcher, ASDF/dependencies, decoder, font and tools).
;; Do not incorporate parallel character-set integration into this declaration.
(defparameter +source-paths+
  '("tests/vttest-area.lisp" "tests/vttest-selection.lisp" "tests/verify.sh"
    "tests/vttest-cursor-driver.lisp" "tests/vttest-protocol.lisp"
    "tests/vttest-cursor-oracle.lisp" "tests/vttest-screen-oracle.lisp"
    "tests/vttest-screen-driver.lisp" "tests/vttest-screen-manifest.lisp"
    "tests/vttest-screen-native-contract.lisp" "tests/vttest-screen-frame.lisp"
    "tests/vttest-double-size-driver.lisp" "tests/vttest-double-size-manifest.lisp"
    "tests/vttest-double-size-native-contract.lisp"
    "tests/vttest-double-size-recorder.lisp" "tests/vttest-double-size-pixels.lisp"
    "tests/vttest-double-size-oracle.lisp" "tests/vttest-double-size-native.sh"
    "tests/vttest-screen-build.sh" "scripts/sbcl" "scripts/deps.nix"
    "scripts/bootstrap.lisp" "lispterm.asd" "src/host.lisp" "src/window.lisp"))

(defun scenarios ()
  (loop for pause from 1 to 6 collect
    (list :id (format nil "double-size-~2,'0d" pause)
          :recorder-label "double-size" :menu-path '(4) :pauses 6
          :captures (list (list :id (format nil "pause-~2,'0d" pause) :pause pause))
          :bytes nil :timing t
          :oracle (list :path +oracle+ :sha256 +oracle-sha256+)
          ;; Built-in native execution, not a capability-ready receipt.
          :requires nil)))

(defun make-manifest (backend &optional (scale 1))
  "Declare all six holds; this does not certify launch or native observations.
The independent oracle covers widths 640,640,1056,1056,640,640 at height384,
area-local single/wide/top/bottom row modes, and cursors23/14 (1..4),23/54
(5),1/53 (6). Holds5/6 contain real blink attributes. Only5 requires phaseT;
6 uses its actual stable boolean, including NIL. No phase selection is allowed."
  (vttest-area::need (member backend '("x11" "wayland") :test #'equal) :unknown-backend)
  (vttest-area::check-scale backend scale)
  (list :kind :vttest-area-manifest :version 1 :area "double-size-characters"
        :backend backend :scale scale :scope-status :frozen
        :conditions (list "private-headless" "source-built-per-run" (format nil "scale-~d" scale)
          "grid-80-132x24" "cell-8x16" "all-6-fixed-holds"
          "normal-default-colors-sgr-screen" "ascii-g0-gl" "full-margins"
          "absolute-cursor-addressing" "autowrap-overwrite" "visible-solid-block-cursor"
          "strict-original-frame-bracket" "pause5-blink-on"
          "actual-stable-blink-phase" "full-independent-row-mode-reference"
          "exact-rgba-all-pixels")
        :scenarios (scenarios) :required-count 6
        :native-driver (format nil "double-size-~a-v1" backend)
        :pins (cons (list :path +archive+ :sha256 +archive-sha256+)
                    (loop for path in +source-paths+
                          collect (list :path path :sha256 (digest path))))))

(defun validate (manifest)
  "Rehash current declared file bytes and require the exact closed declaration.
Conditions describe the existing public launch profile, not private attestation."
  (vttest-area::need (equal (digest +archive+) +archive-sha256+) :double-size-archive-mismatch)
  (vttest-area::need (equal (digest +oracle+) +oracle-sha256+) :double-size-oracle-mismatch)
  (vttest-area::need (equal manifest (make-manifest (getf manifest :backend) (getf manifest :scale)))
                    :double-size-manifest-mismatch)
  manifest)
