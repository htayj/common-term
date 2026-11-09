;;;; Offline PNG validation controls. CAPTURE-PIXELS runs only with stubbed
;;;; *PIXEL-TOOL* hooks: the fixture magick stand-in and project tools never run.
;;;; Load after vttest-keyboard-png.lisp; the fixtures are loaded when absent.
(in-package :vttest-keyboard-driver-tests)

(unless (fboundp 'run-png)
  (load (merge-pathnames "vttest-keyboard-png-fixtures.lisp"
                         (or *load-truename* *default-pathname-defaults*))))

(defun png-try (&key (png (png-header)) (expected (opaque-rgba)) (raw (opaque-rgba))
                     identify decode after hook (mode #o755) link magick)
  "RUN-PNG on a fresh root plus :ROOT. PNG is octets for run/x.png, NIL for none,
or a function of the run/ directory; MAGICK, when given, is a function of the root."
  (let* ((root (png-root)) (run (c::cat root "run/")))
    (cond ((functionp png) (funcall png run)) (png (write-png run "x" png)))
    (list* :root root
           (run-png root "x" expected
                    :hook (or hook (apply #'png-hook :raw raw :after-decode after
                                          (append (and identify (list :identify identify))
                                                  (and decode (list :decode decode)))))
                    :magick (if magick
                                (funcall magick root)
                                (fixture-magick root :mode mode :link link))))))

(defun png-reason-p (r reasons)
  "The first reason of R is, or is a list headed by, a member of REASONS."
  (let ((x (first (getf r :reasons))))
    (member (if (consp x) (car x) x) reasons)))

(defun png-path (argv) (subseq (second argv) 4))

(defun png-controls ()
  "Stubbed CAPTURE-PIXELS controls; true when none failed."
  (let ((before (length *failures*)) (tool k::*pixel-tool*) (magick k::*magick*)
        (clear (opaque-rgba)) (short (subseq (opaque-rgba) 4)) (tail (opaque-rgba)))
    (incf (aref tail (- (length tail) 4)))
    (setf (aref clear (+ 3 (* 4 (+ 7 (* 640 5))))) 0)
    (let* ((r (png-try)) (root (getf r :root)) (png (c::cat root "run/x.png"))
           (exe (c::cat root "bin/magick")) (argv (getf r :argv))
           (full (if (char= #\/ (char exe 0)) exe (c::cat (sb-posix:getcwd) "/" exe))))
      (check (and (eq :pass (getf r :status)) (null (getf r :reasons))
                  (= 2 (getf r :tools) (length argv)) (= 1 (getf r :comparisons)))
             :png-pass-whole-raster)
      (check (and (equal (getf r :sha) (png-sha png)) (equalp (raw-octets png) (png-header))
                  (equalp (raw-octets (getf r :raw)) (opaque-rgba))
                  (eql 0 (search (c::cat root "scratch/keyboard-pixels-") (getf r :raw))))
             :png-hash-and-retained-raw)
      (check (and (equal (first argv) (list full "identify" "-format" "%w %h" (c::cat "png:" png)))
                  (equal (second argv) (list full (c::cat "png:" png) "-depth" "8"
                                             (c::cat "rgba:" (getf r :raw)))))
             :png-same-absolute-executable))
    (let* ((r (png-try :link t)) (exe (c::cat (getf r :root) "bin/magick-link")))
      (check (and (eq :pass (getf r :status)) (search exe (car (first (getf r :argv))))
                  (equal (car (first (getf r :argv))) (car (second (getf r :argv)))))
             :png-nix-style-link-accepted))
    (check (eq :pass (getf (png-try :png (png-header :colour 2)) :status)) :png-rgb-header)
    (let ((r (png-try :magick (lambda (root) (c::cat root "bin/absent")))))
      (check (and (eq :unverified (getf r :status)) (equal '(:missing-magick) (getf r :reasons))
                  (getf r :sha) (= 0 (getf r :tools)))
             :png-absent-magick-unverified))
    (dolist (c `((:missing-png :unverified (:missing-png) 0 0 :png nil)
                 (:png-symlink :fail (:file) 0 0
                  :png ,(lambda (d) (sb-posix:symlink (write-png d "real") (c::cat d "x.png"))))
                 (:non-executable :fail (:magick-not-executable) 0 0 :mode #o644)
                 (:link-to-non-executable :fail (:magick-not-executable) 0 0 :mode #o644 :link t)
                 (:directory-magick :fail (:magick-not-executable) 0 0
                  :magick ,(lambda (root) (c::cat root "run")))
                 (:dangling-magick :fail (:magick-not-executable) 0 0
                  :magick ,(lambda (root) (let ((l (c::cat root "dangle")))
                                           (sb-posix:symlink (c::cat root "nowhere") l) l)))
                 (:full-mismatch :fail (:different-pixels) 2 1 :raw ,(opaque-rgba 1))
                 (:last-pixel :fail (:different-pixels) 2 1 :raw ,tail)
                 (:width :fail (:png-header) 0 0 :png ,(png-header :width 641))
                 (:height :fail (:png-header) 0 0 :png ,(png-header :height 383))
                 (:depth-16 :fail (:png-header) 0 0 :png ,(png-header :depth 16))
                 (:grey-alpha :fail (:png-header) 0 0 :png ,(png-header :colour 4))
                 (:palette :fail (:png-header) 0 0 :png ,(png-header :colour 3))
                 (:not-png :fail (:png-header) 0 0 :png ,(png-octets '(1 2 3)))
                 (:expected-alpha :fail (:expected-alpha) 0 0 :expected ,clear)
                 (:decoded-alpha :fail (:translucent) 2 0 :raw ,clear)
                 (:expected-list :fail (:expected-raster) 0 0 :expected ,(coerce clear 'list))
                 (:expected-untyped :fail (:expected-raster) 0 0
                  :expected ,(coerce (opaque-rgba) 'simple-vector))
                 (:expected-short-before-png :fail (:expected-raster) 0 0 :png nil :expected ,short)
                 (:decoded-short :fail (:decoded-length) 2 0 :raw ,short)
                 (:decoded-empty :fail (:decoded-length) 2 0 :raw ,(png-octets nil))
                 (:identify-1 :fail (:identify) 1 0 :identify ("640 384" 1 ""))
                 (:identify-126 :fail (:identify) 1 0 :identify ("640 384" 126 ""))
                 (:identify-no-exit :fail (:identify) 1 0 :identify ("640 384" nil ""))
                 (:identify-stderr :fail (:identify) 1 0 :identify ("640 384" 0 "warn"))
                 (:identify-size :fail (:identify-size) 1 0 :identify ("641 384" 0 ""))
                 (:decode-127 :fail (:decode) 2 0 :decode ("" 127 ""))
                 (:decode-stderr :fail (:decode) 2 0 :decode ("" 0 "x"))
                 (:hook-error :fail (:tool-error) 1 0
                  :hook ,(lambda (argv) (declare (ignore argv)) (error "stub")))
                 (:png-rewritten :fail (:png-changed :png-identity-changed) 2 0
                  :after ,(lambda (a) (put-octets (png-path a) (png-header :colour 2))))
                 (:png-grown :fail (:png-changed :png-identity-changed) 2 0
                  :after ,(lambda (a) (with-open-file (s (png-path a) :direction :output :if-exists :append
                                                            :element-type '(unsigned-byte 8))
                              (write-byte 0 s))))
                 (:png-renamed-same-octets :fail (:png-identity-changed) 2 0
                  :after ,(lambda (a) (let ((p (png-path a)))
                                        (sb-posix:rename (put-octets (c::cat p ".new") (png-header)) p))))
                 (:png-removed :fail (:png-identity-changed) 2 0
                  :after ,(lambda (a) (delete-file (png-path a))))))
      (destructuring-bind (label status reasons tools comparisons &rest keys) c
        (let ((r (apply #'png-try keys)))
          (check (and (eq status (getf r :status)) (png-reason-p r reasons)
                      (= tools (getf r :tools)) (= comparisons (getf r :comparisons))
                      (<= (length (getf r :argv)) tools))
                 (list :png label)))))
    (check (and (eq tool k::*pixel-tool*) (equal magick k::*magick*)) :png-bindings-restored)
    (= before (length *failures*))))
