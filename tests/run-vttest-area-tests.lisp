;;;; Standalone runner, without project bootstrap or native dependencies.
(load "tests/vttest-area.lisp")
(load "tests/vttest-area-tests.lisp")
(load "tests/vttest-cursor-driver-tests.lisp")
(sb-ext:exit :code (if (and (vttest-area-tests:run) (vttest-cursor-driver-tests:run)) 0 1))
