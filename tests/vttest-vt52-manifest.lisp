;;;; VT52 capture declaration, separate from native evidence.
(in-package #:vttest-vt52-driver)

(defparameter +vt52-bash+ vttest-charsets-driver::+charsets-bash+)
(defparameter +vt52-archive+ vttest-charsets-driver::+charsets-archive+)
(defparameter +vt52-source-sha+ vttest-charsets-driver::+charsets-source-sha+)
;; Extend the explicit audited shared closure, never collect repository paths.
;; The committed tree plus explicit inputs bind repository source.
;; External Nix runtime libraries are not exhaustively individually hashed.
(defparameter +vt52-pins+
  (remove-duplicates
   (append vttest-charsets-driver::+charsets-pins+
    '("tests/vttest-vt52-driver.lisp" "tests/vttest-vt52-manifest.lisp"
      "tests/vttest-vt52-oracle.lisp" "tests/vttest-vt52-pixels.lisp"
      "tests/vttest-vt52-sha256.lisp" "tests/vttest-vt52-admission.lisp"
      "tests/vttest-vt52-native-check.lisp" "tests/vttest-vt52-png-adapter.lisp"
      "tests/vttest-vt52-capture-adapter.lisp" "tests/vttest-vt52-navigation.lisp"
      "tests/vttest-vt52-native-io.lisp" "tests/vttest-vt52-native-root.lisp"
      "tests/vttest-vt52-native-lifecycle.lisp" "tests/vttest-vt52-native-contract.lisp"
      "tests/vttest-vt52-native-provenance.lisp" "tests/vttest-vt52-native-clock.lisp"
      "tests/vttest-vt52-native-navigation.lisp"
      "tests/vttest-screen-blink-native.lisp" "tests/vttest-screen-blink-selection.lisp"
      "tests/vttest-screen-blink-timing.lisp" "tests/vttest-screen-blink-contract.lisp"
      "tests/vttest-double-size-driver.lisp"
      "tests/vttest-double-size-native-contract.lisp"
      "tests/vttest-double-size-manifest.lisp"
      "tests/vttest-keyboard-driver.lisp"
      "tests/vttest-keyboard-native-contract.lisp"
      "tests/vttest-keyboard-artifacts.lisp"
      "tests/vttest-keyboard-timing.lisp"
      "tests/vttest-keyboard-manifest.lisp"
      "tests/vttest-keyboard-cleanup.lisp"
      "tests/vttest-keyboard-native-summary.lisp"
      "tests/vttest-keyboard-prefix.lisp"
      "tests/vttest-keyboard-cleanup-schema.lisp"
      "tests/vttest-keyboard-cleanup-status.lisp"
      "/nix/store/9qvd27vg12177prxcl778rdhxh4lk2mi-terminus-font-4.49.1/share/fonts/terminus/ter-u16n.otb"
      "/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick"))
   :test #'equal))

(defun vt52-digest (path)
  (vttest-area::sha256 (vttest-cursor-driver::bytes path)))

(defun make-manifest (backend &optional (scale 1))
  (vttest-area::need (member backend '("x11" "wayland") :test #'equal) :unknown-backend)
  (vttest-area::check-scale backend scale)
  (list :kind :vttest-area-manifest :version 1 :area "vt52-mode"
        :backend backend :scale scale :navigation-pauses '(1 2 3) :scope-status :frozen
        :conditions '("private-headless" "source-built-per-run"
                      "one-original-pause-02" "navigation-pauses-1-2-3"
                      "exact-rgba-all-pixels" "fresh-native-custody")
        :scenarios
        (list (list :id "vt52-01" :recorder-label "vt52" :menu-path '(7) :pauses 3
                    :captures (list (list :id "pause-02" :pause 2))
                    :bytes nil :timing t
                    :oracle (list :path "tests/vttest-vt52-oracle.lisp"
                                  :sha256 (vt52-digest "tests/vttest-vt52-oracle.lisp"))
                    :requires nil))
        :required-count 1 :native-driver (format nil "vt52-~a-v1" backend)
        :pins (cons (list :path +vt52-archive+ :sha256 +vt52-source-sha+)
                    (loop for path in +vt52-pins+
                          collect (list :path path :sha256 (vt52-digest path))))))

(defun validate (manifest)
  (vttest-area::need (equal (vt52-digest +vt52-archive+) +vt52-source-sha+)
                    :vt52-source-archive-mismatch)
  (vttest-area::need
   (equal manifest (make-manifest (getf manifest :backend) (getf manifest :scale)))
   :vt52-manifest-mismatch)
  manifest)
