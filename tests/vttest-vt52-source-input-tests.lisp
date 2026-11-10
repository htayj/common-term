;;;; Real manifest declarations versus the shell's actual expanded operands.
(load (merge-pathnames "vttest-vt52-source-input-test-support.lisp" *load-truename*))
(load (merge-pathnames "vttest-vt52-source-pins.lisp" *load-truename*))
(in-package :vttest-vt52-source-input-tests)
(defun replace-one (text old new)
  (let ((at (or (search old text) (error "Missing mutation target"))))
    (concatenate 'string (subseq text 0 at) new (subseq text (+ at (length old))))))
(let* ((pins (manifest-paths)) (paths (shell-paths)) (text (shell-text)) (checks 0))
  (flet ((check (value) (incf checks) (assert value)))
    (check (vttest-vt52-source-pins:expected-p pins))
    (check (= (length pins) (length (remove-duplicates pins :test #'equal))))
    (dolist (p '("tests/vttest-vt52-source-pins.lisp" "tests/vttest-vt52-native-trace.lisp"))
      (check (= 1 (count p pins :test #'equal)))
      (check (= 1 (count p paths :test #'equal))))
    (check (null (missing-pins)))
    ;; Every real declaration must match the exact checksum operand spelling.
    (dolist (pin pins) (check (member pin paths :test #'equal)))
    (check (equal (list (first pins))
                  (missing-pins (replace-one text "archive_input=${archive#\"$root/\"}"
                                             "archive_input=$archive"))))
    (check (equal '("tests/vttest-protocol.lisp")
                  (missing-pins (replace-one text "tests/vttest-protocol.lisp " ""))))
    ;; The shared area loader reaches each of these repository sources.
    (let ((loaded-paths '("tests/vttest-double-size-driver.lisp"
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
                          "tests/vttest-keyboard-cleanup-status.lisp")))
      (dolist (path loaded-paths)
        (check (= 1 (count path pins :test #'equal)))
        (check (= 1 (count path paths :test #'equal)))
        (check (equal (list path) (missing-pins (replace-one text path "")))))
      (check (equal (shell-paths text "charsets")
                    (shell-paths (reduce (lambda (s path) (replace-one s path ""))
                                         loaded-paths :initial-value text)
                                 "charsets"))))
    (format t "VT52 source-input mapping PASS~D: real-pins=~D shell-operands=~D missing=0~%"
            checks (length pins) (length paths))))
