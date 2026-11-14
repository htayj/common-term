(in-package #:lispterm.gpu.tests)

(defparameter *cpu-baseline* "56ca10b1367177b20d649f8144280a2fa20f3b02")
(defparameter *existing-glue*
  '("tests/verify.sh" "scripts/sbcl" "scripts/deps.nix"
    "tests/packaging-gate.sh" "tests/wayland-gate.sh" "tests/vttest-gate.sh"
    "tests/fastfetch-gate.sh" "scripts/autolith-runtime"))

(defun nul-paths (text)
  (remove "" (uiop:split-string text :separator '(#\Null)) :test #'equal))

(defun allowed-authored-path-p (path existing-p)
  (or (member (pathname-type path) '("lisp" "asd" "org" "md") :test #'equal)
      (and existing-p (member path *existing-glue* :test #'equal))))

(defun check-authored-paths (root)
  ;; Include committed, staged, unstaged and nonignored untracked changes.
  (let* ((baseline (nul-paths (git-output root "ls-tree" "-r" "--name-only" "-z"
                                         *cpu-baseline*)))
         (changed (nul-paths (git-output root "diff" "--name-only" "-z"
                                        "--diff-filter=ACMRT" *cpu-baseline* "--")))
         (untracked (nul-paths (git-output root "ls-files" "--others" "--exclude-standard" "-z"))))
    (dolist (path (union changed untracked :test #'equal))
      (check (allowed-authored-path-p path (member path baseline :test #'equal))
             "Migration contains disallowed authored file: ~A" path)))
  t)

(register-check "gpu-language" (lambda () (check-authored-paths *root*)))
