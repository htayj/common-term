(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm/tests")
(unless (cl-weave:run-all :reporter :spec :max-workers 1 :pass-with-no-tests nil)
  (uiop:quit 1))
