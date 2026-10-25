(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm")
(load (merge-pathnames "host-input-tests.lisp" *load-truename*))
(lispterm.input.tests:run-tests)
