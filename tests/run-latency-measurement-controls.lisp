;;;; tests/verify.sh latency-measurement-controls [--tmux]
;;;; Needs no display endpoint and no tmux. With --tmux it also checks the private
;;;; tmux helper: it starts one private tmux server and removes it.
(load (merge-pathnames "../scripts/bootstrap.lisp" *load-truename*))
(asdf:load-system "lispterm")
(dolist (file (if (member "--tmux" sb-ext:*posix-argv* :test #'string=)
                  '("latency-tokens" "latency-measure" "latency-tmux"
                    "latency-measurement-controls" "latency-audit-controls" "latency-tmux-controls")
                  '("latency-tokens" "latency-measure" "latency-measurement-controls"
                    "latency-audit-controls")))
  (load (merge-pathnames (concatenate 'string file ".lisp") *load-truename*)))
(sb-ext:exit :code (if (lispterm.latency.controls:run) 0 1) :abort t)
