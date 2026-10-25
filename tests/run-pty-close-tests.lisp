;;;; scripts/sbcl --script tests/run-pty-close-tests.lisp
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system :cffi)
(let ((root (merge-pathnames "../" *load-truename*)))
  (dolist (file '("package" "ffi" "posix" "process"))
    (load (merge-pathnames (format nil "src/pty/~A.lisp" file) root))))
(load (merge-pathnames "pty-close-concurrency-tests.lisp" *load-truename*))
(lispterm.pty.close-tests::run-tests)
