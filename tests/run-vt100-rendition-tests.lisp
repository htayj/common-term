(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm")
(load (merge-pathnames "vt100-rendition-tests.lisp" *load-truename*))
(lispterm.rendition.tests:run-tests)
