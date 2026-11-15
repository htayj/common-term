;;;; Raw terminal timing with separate native DMA positive/negative audits.
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
;; Reject implicit/unknown modes before loading or launching native fixtures.
(defparameter *raw-latency-mode*
  (let ((arguments (uiop:command-line-arguments)))
    (cond ((equal arguments '("baseline-record")) :baseline-record)
          ((equal arguments '("fixed-acceptance")) :fixed-acceptance)
          (t (format *error-output*
                     "Usage: latency-wayland {baseline-record|fixed-acceptance}~%")
             (uiop:quit 2)))))
(defun raw-latency-source-hashes ()
  (let* ((root (asdf:system-source-directory "lispterm"))
         (tracked (uiop:run-program '("git" "ls-files" "-z" "--" "src/" "engine/" "scripts/" "lispterm.asd")
                                   :directory root :input nil :output :string :error-output *error-output*))
         (files (remove "" (uiop:split-string tracked :separator '(#\Null)) :test #'equal))
         (helpers (mapcar #'namestring (uiop:directory-files (merge-pathnames "tests/" root) "latency-*.lisp"))))
    (uiop:run-program (append '("sha256sum" "--") files helpers '("tests/run-latency-wayland.lisp"))
                      :directory root :input nil :output :string :error-output *error-output*)))
(defparameter *raw-latency-load-hashes* (raw-latency-source-hashes))
(asdf:load-system "lispterm/gpu-tests")
(dolist (file '("latency-tokens" "latency-measure" "latency-budget-controls"
                "latency-raw-wayland" "latency-dma-audit"))
  (load (merge-pathnames (concatenate 'string file ".lisp") *load-truename*)))
(handler-case
    (let ((outcome (lispterm.latency.raw:run :mode *raw-latency-mode*)))
      ;; 0 means fixed budgets passed; 3 means a valid baseline was recorded.
      (uiop:quit (case outcome (:pass 0) (:baseline-recorded 3) (otherwise 1))))
  (error (condition)
    (format *error-output* "Raw latency measurement failed: ~A~%" condition)
    (uiop:quit 1)))
