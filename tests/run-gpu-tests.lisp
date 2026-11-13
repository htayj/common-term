(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm/gpu-tests")
(handler-case
    (let ((args (uiop:command-line-arguments)))
      (unless (= 1 (length args)) (error "Expected exactly one GPU gate name"))
      (lispterm.gpu.tests:run-check (first args)))
  (error (condition)
    (format *error-output* "GPU verification failed: ~A~%" condition)
    (uiop:quit 1)))
