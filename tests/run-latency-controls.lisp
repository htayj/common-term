;;;; tests/verify.sh latency-controls: offline admission and frozen budget checks.
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm")
(dolist (file '("latency-tokens" "latency-measure" "latency-budget-controls"))
  (load (merge-pathnames (concatenate 'string file ".lisp") *load-truename*)))
(sb-ext:exit :code (if (lispterm.latency.budgets:run) 0 1) :abort t)
