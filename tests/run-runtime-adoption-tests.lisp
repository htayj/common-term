;;;; Run with scripts/autolith-runtime --script tests/run-runtime-adoption-tests.lisp.
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "autolith")
(asdf:load-system "lispterm")
(load (merge-pathnames "../src/autolith-adapter.lisp" *load-truename*))
;; Disposable image only: fake display transport, real PTY/process/window owner.
(load (merge-pathnames "host-tests.lisp" *load-truename*))
(load (merge-pathnames "runtime-adoption-tests.lisp" *load-truename*))
(lispterm.runtime-adoption.tests::run-tests)
