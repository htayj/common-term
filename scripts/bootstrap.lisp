;;;; No user init/Quicklisp required. Vendor framework and packaged Nix systems.
(require :asdf)
(let* ((root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-directory-pathname *load-truename*))))
  (pushnew root asdf:*central-registry* :test #'equal)
  (dolist (name '("cl-weave" "xclhb" "struct-plus"))
    (let ((directory (merge-pathnames (format nil "vendor/~A/" name) root)))
      (when (probe-file directory) (pushnew directory asdf:*central-registry* :test #'equal))))
  (asdf:load-asd (merge-pathnames "lispterm.asd" root))
(asdf:load-asd (merge-pathnames "common-term.asd" root)))
