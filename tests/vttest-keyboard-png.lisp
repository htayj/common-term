;;;; Keyboard PNG validation: one no-follow, hash-stable 640x384 8-bit RGB/RGBA
;;;; capture, decoded by magick into retained scratch and compared exactly at
;;;; scale 1 with caller-supplied opaque RGBA8. Needs vttest-area, the cursor
;;;; driver and oracle, the keyboard driver (present-p, artifact, need) and UIOP.
;;;; Limit: magick opens the PNG by name, so the before/after snapshots catch a
;;;; lasting change but not a swap restored with identical identity and octets.
(in-package #:vttest-keyboard-driver)

(defconstant +png-width+ 640)
(defconstant +png-height+ 384)
(defconstant +rgba-length+ (* 4 640 384))

(defvar *magick* ".build/test-tools/bin/magick"
  "Decoder executable, relative to the process working directory.")
(defvar *pixel-tool* 'pixel-run
  "Decoder hook called with (EXECUTABLE . ARGS); returns stdout, exit status, stderr.")
(defvar *pixel-tool-calls* 0 "Decoder hook calls, counted before each call.")
(defvar *pixel-comparisons* 0 "Complete raster comparisons started.")

(defun pixel-cap (list) (subseq list 0 (min 8 (length list))))
(defun pixel-clip (x) (if (stringp x) (subseq x 0 (min 256 (length x))) x))

(defun pixel-run (argv)
  "Run timeout --kill-after=1 60 ARGV without stdin.
Values: stdout, exit status, stderr."
  (multiple-value-bind (out err exit)
      (uiop:run-program (list* "timeout" "--kill-after=1" "60" argv) :input nil
                        :output :string :error-output :string :ignore-error-status t)
    (values out exit err)))

(defun magick-path ()
  "*MAGICK* as the absolute path both checked and launched."
  (need (and (stringp *magick*) (plusp (length *magick*))) :magick-path)
  (if (char= (char *magick* 0) #\/)
      *magick*
      (concatenate 'string (sb-posix:getcwd) "/" *magick*)))

(defun magick-state (exe)
  ":MISSING only on lstat ENOENT; :EXECUTABLE when EXE or its link target (such as
a Nix store file) is a regular executable file; else :NOT-EXECUTABLE."
  (handler-case (sb-posix:lstat exe)
    (sb-posix:syscall-error (e)
      (if (= (sb-posix:syscall-errno e) sb-posix:enoent)
          (return-from magick-state :missing)
          (error e))))
  (handler-case
      (if (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:stat exe)))
          (progn (sb-posix:access exe sb-posix:x-ok) :executable)
          :not-executable)
    (sb-posix:syscall-error () :not-executable)))

(defun png-snapshot (path)
  "Identity (dev ino size mtime), SHA-256 and PNG-HEAD of PATH's octets, all
from one no-follow descriptor."
  (let ((fd (vttest-area::open-input-fd path)))
    (with-open-stream (s (sb-sys:make-fd-stream fd :input t :auto-close t
                                                   :element-type '(unsigned-byte 8)))
      (let* ((st (sb-posix:fstat fd))
             (v (make-array (sb-posix:stat-size st) :element-type '(unsigned-byte 8))))
        (need (and (= (read-sequence v s) (length v)) (null (read-byte s nil)))
              :png-size-changed)
        (values (list (sb-posix:stat-dev st) (sb-posix:stat-ino st)
                      (sb-posix:stat-size st) (sb-posix:stat-mtime st))
                (vttest-area::sha256 v) (png-head v))))))

(defun png-head (octets)
  "Width, height, bit depth and colour type from signature and IHDR, or NIL."
  (when (and (>= (length octets) 26)
             (equalp (subseq octets 0 16) #(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82)))
    (flet ((u32 (p) (loop for i below 4 sum (ash (aref octets (+ p i)) (* 8 (- 3 i))))))
      (list (u32 16) (u32 20) (aref octets 24) (aref octets 25)))))

(defun capture-pixels (dir name expected scratch)
  "Validate DIR NAME.png against EXPECTED, opaque 640x384 RGBA8 octets. The
raster is decoded into a fresh mkdtemp directory under SCRATCH (ending in /)
and retained; the PNG is only read. Values: :PASS, :FAIL or :UNVERIFIED; at
most 8 reasons; PNG SHA-256 or NIL; raw RGBA path once decoded, else NIL.
:UNVERIFIED means only an absent PNG or magick (lstat ENOENT)."
  (let ((hash nil) (raw nil))
    (labels ((done (status &rest reasons)
               (return-from capture-pixels (values status (pixel-cap reasons) hash raw)))
             (tool (label &rest argv)
               ;; Hook errors, a non-zero or non-numeric exit, or any stderr fail.
               (incf *pixel-tool-calls*)
               (multiple-value-bind (out exit err)
                   (handler-case (funcall *pixel-tool* argv)
                     (error (e) (done :fail (list :tool-error (type-of e)
                                                  (pixel-clip (princ-to-string e))))))
                 (unless (and (eql exit 0) (equal err ""))
                   (done :fail (list label exit (pixel-clip out) (pixel-clip err))))
                 out)))
      (handler-case
          (progn
            (unless (and (typep expected '(vector (unsigned-byte 8)))
                         (= (length expected) +rgba-length+))
              (done :fail :expected-raster))
            (let ((alpha (vttest-cursor-oracle::first-translucent expected +png-width+)))
              (when alpha (done :fail (list :expected-alpha alpha))))
            (let* ((path (artifact dir name ".png"))
                   (png (concatenate 'string "png:" path))
                   (exe (magick-path))
                   (identity nil))
              (unless (present-p path) (done :unverified :missing-png))
              (multiple-value-bind (id sha head) (png-snapshot path)
                (setf identity id hash sha)
                (unless (and (eql (first head) +png-width+) (eql (second head) +png-height+)
                             (eql (third head) 8) (member (fourth head) '(2 6)))
                  (done :fail (list :png-header head))))
              (case (magick-state exe)
                (:missing (done :unverified :missing-magick))
                (:not-executable (done :fail (list :magick-not-executable exe))))
              (let ((out (tool :identify exe "identify" "-format" "%w %h" png)))
                (unless (equal out (format nil "~D ~D" +png-width+ +png-height+))
                  (done :fail (list :identify-size (pixel-clip out)))))
              (let* ((tmp (sb-posix:mkdtemp (concatenate 'string scratch "keyboard-pixels-XXXXXX")))
                     (file (concatenate 'string tmp "/actual.rgba")))
                (tool :decode exe png "-depth" "8" (concatenate 'string "rgba:" file))
                (setf raw file))
              (unless (present-p path) (done :fail :png-identity-changed))
              (multiple-value-bind (id sha) (png-snapshot path)
                (unless (equal identity id) (done :fail :png-identity-changed))
                (unless (equal hash sha) (done :fail :png-changed)))
              (let ((rgba (vttest-cursor-driver::bytes raw)))
                (unless (= (length rgba) +rgba-length+)
                  (done :fail (list :decoded-length (length rgba))))
                (let ((translucent (vttest-cursor-oracle::first-translucent rgba +png-width+)))
                  (when translucent (done :fail (list :translucent translucent))))
                (incf *pixel-comparisons*)
                (let ((c (vttest-cursor-oracle:compare-physical
                          expected rgba +png-width+ +png-height+ 1)))
                  (if (getf c :pass)
                      (done :pass)
                      (done :fail (list :different-pixels (getf c :different-pixels))
                            (list :cells (pixel-cap (getf c :different-cells)))
                            (list :first (getf c :first-difference))
                            (list :last (getf c :last-difference))))))))
        (vttest-cursor-oracle:physical-comparison-refusal (e)
          (done :fail (list :refused (vttest-cursor-oracle:physical-refusal-reason e)
                            (vttest-cursor-oracle:physical-refusal-details e))))
        (vttest-area:gate-problem (e) (done :fail (list :file (vttest-area:problem-reason e))))
        (error (e) (done :fail (list :error (pixel-clip (princ-to-string e)))))))))
