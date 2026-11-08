;;;; Independent vttest 20251205 character-set reference, ANSI/VT100 seven-bit GL.
;;;; Basis: character-sets-xterm-recovery-357-20261108-a/SOURCE-REFERENCE.md.
;;;; Bare SCS 1/2 use ASCII output: explicit matches-xterm emulator policy,
;;;; xterm-412 commit 5c75ead9a75d4b2e666ca06fa94503b5766ed93f,
;;;; charsets.c:421-424,585; not a claim about absent physical DEC ROMs.
(load (merge-pathnames "vttest-screen-oracle.lisp" *load-truename*))

(defpackage #:vttest-charsets-oracle
  (:use #:cl)
  (:export #:character-set-snapshot #:snapshot-cell #:snapshot-cells
           #:snapshot-metadata #:snapshot-rows #:snapshot-columns #:snapshot-pause
           #:invalid-input #:invalid-input-field #:invalid-input-value
           #:require-complete #:map-byte #:admitted-glyph-rows #:resolved-cell-rgba
           #:rasterize-snapshot #:compare-snapshot))
(in-package #:vttest-charsets-oracle)

(define-condition invalid-input (error)
  ((field :initarg :field :reader invalid-input-field)
   (value :initarg :value :reader invalid-input-value))
  (:report (lambda (condition stream)
             (format stream "Outside character-set reference profile: ~s = ~s"
                     (invalid-input-field condition) (invalid-input-value condition)))))

(defun need (predicate field value)
  (unless predicate (error 'invalid-input :field field :value value)))

(defstruct (snapshot (:constructor %snapshot (cells)) (:copier nil)
                     (:conc-name %snapshot-))
  (cells nil :read-only t))

(defun snapshot-rows (snapshot) (check-type snapshot snapshot) 24)
(defun snapshot-columns (snapshot) (check-type snapshot snapshot) 80)
(defun snapshot-pause (snapshot) (check-type snapshot snapshot) 1)
(defun snapshot-cells (snapshot)
  "Fresh row-major vector of immutable approved single-width cells."
  (check-type snapshot snapshot)
  (copy-seq (%snapshot-cells snapshot)))
(defun snapshot-cell (snapshot row column)
  "One-based coordinates, returning an immutable cell."
  (check-type snapshot snapshot)
  (need (typep row '(integer 1 24)) :row row)
  (need (typep column '(integer 1 80)) :column column)
  (aref (%snapshot-cells snapshot) (+ (* (1- row) 80) (1- column))))

(defun snapshot-metadata (snapshot)
  "Fresh conditional reference contract, not native capture admission."
  (check-type snapshot snapshot)
  (list :status :complete-reference :area (copy-seq "character-sets") :label (copy-seq "charsets")
        :path (list 3) :pause 1 :rows 24 :columns 80 :width 640 :height 384
        ;; Upstream footer40 + holdit13 occupy columns1..53, next cursor54.
        :cursor (list :row 24 :column 54 :visible t)
        :screen-reverse nil :pending-wrap nil :origin nil :margins (list 1 24)
        :current-pen (list :bold nil :underline nil :blink nil :inverse nil
                           :foreground :default :background :default)
        :current-charset (list :g0 :ascii :g1 :ascii :invoked :g0)
        :alternate-rom-policy :matches-xterm-ascii
        :xterm-commit (copy-seq "5c75ead9a75d4b2e666ca06fa94503b5766ed93f")
        :source-charsets-sha256 (copy-seq "0b33ebeb34382620e526e470f8ecf90cc5e779bc604187ab957ca7a0fd0fa0e9")
        :printable-samples 950 :ignored-del 10
        :requirements (list :vt100 :seven-bit :normal-sgr :normal-screen
                            :80x24 :single-lines :full-margins :absolute-origin
                            :highlighting-disabled :observed-boolean-blink-phase)))

(defun map-byte (designator byte)
  "Codepoint/:RESOLVED for printable samples, NIL/:IGNORED for DEL.
DEC B/A/0 plus the source-proven matches-xterm ASCII policy for bare 1/2."
  (need (typep byte '(integer 32 127)) :byte byte)
  (need (member designator '(#\B #\A #\0 #\1 #\2)) :designator designator)
  (cond ((= byte 127) (values nil :ignored))
        ((and (char= designator #\A) (= byte 35)) (values #xa3 :resolved))
        ((and (char= designator #\0) (>= byte #x5f))
         (values (aref #(#x20 #x25c6 #x2592 #x2409 #x240c #x240d #x240a #xb0
                        #xb1 #x2424 #x240b #x2518 #x2510 #x250c #x2514 #x253c
                        #x23ba #x23bb #x2500 #x23bc #x23bd #x251c #x2524 #x2534
                        #x252c #x2502 #x2264 #x2265 #x3c0 #x2260 #xa3 #xb7)
                       (- byte #x5f)) :resolved))
        (t (values byte :resolved))))

(defun character-set-snapshot ()
  "All 1920 cells of the single pause; no terminal parser or captured grid.
charsets.c:138-144,494-530; esc.c:1299-1310; main.c:1740-1747;
unix_io.c:237-241. Highlighting is disabled by tst_characters:957-958."
  (let ((cells (make-array 1920 :initial-element (vttest-screen-oracle:make-cell))))
    (labels ((put (row column cell)
               (setf (aref cells (+ (* (1- row) 80) (1- column))) cell))
             (text (row column string &optional bold)
               (loop for ch across string for col from column
                     do (put row col (vttest-screen-oracle:make-cell
                                      :code (char-code ch) :bold bold)))))
      (text 1 10 "Selected as G0 (with SI)")
      (text 1 48 "Selected as G1 (with SO)")
      (loop for designator across "BA012"
            for name in '("US ASCII" "British" "DEC Special graphics and line drawing"
                          "DEC Alternate character ROM standard characters"
                          "DEC Alternate character ROM special graphics")
            for row from 3 by 4 do
        (text row 1 (format nil "Character set ~c (~a)" designator name) t)
        (dotimes (bank 2)
          (loop for byte from 32 to 126 do
            (put (+ row (floor byte 32)) (+ 10 (* bank 38) (mod byte 32))
                 (vttest-screen-oracle:make-cell :code (map-byte designator byte))))))
      (text 24 1 "These are the installed character sets. Push <RETURN>"))
    (%snapshot cells)))

(defun require-complete (snapshot)
  "Validate complete geometry and every cell, never omit an optional bank."
  (check-type snapshot snapshot)
  (need (and (vectorp (%snapshot-cells snapshot))
             (= 1920 (length (%snapshot-cells snapshot)))) :cell-count :invalid)
  (loop for cell across (%snapshot-cells snapshot) do
    (need (and (typep cell 'vttest-screen-oracle::cell)
               (= 1 (vttest-screen-oracle:cell-width cell))
               (eq :single (vttest-screen-oracle:cell-line-size cell))) :cell cell))
  snapshot)

(defun admitted-glyph-rows (font code)
  "Nonzero cmap and a valid actual 8x16 glyph record, using the approved decoder.
The public raster entry points separately require caller-authenticated font SHA."
  (need (and (typep code '(integer 32 65535))
             (not (<= #xd800 code #xdfff))) :code code)
  (handler-case
      (let ((glyph (vttest-cursor-oracle::cmap-glyph font code)))
        (need (and (integerp glyph) (plusp glyph)) :glyph glyph)
        (vttest-cursor-oracle:glyph-rows font (code-char code)))
    (invalid-input (condition) (error condition))
    (error () (error 'invalid-input :field :glyph-record :value code))))

(defun admit-raster-input (snapshot font font-sha256 phase phase-p)
  (need (and phase-p (or (eq phase t) (eq phase nil))) :blink-phase phase)
  (need (equal font-sha256 "0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca")
        :font-sha256 font-sha256)
  (require-complete snapshot)
  (handler-case (vttest-cursor-oracle:font-info font)
    (error () (error 'invalid-input :field :font-layout :value :invalid))))

(defun resolved-cell-rgba (snapshot row column font
                           &key font-sha256 (blink-phase nil phase-p))
  "Approved attributed tile, with cursor inversion at the observed phase only."
  (admit-raster-input snapshot font font-sha256 blink-phase phase-p)
  (let ((cell (snapshot-cell snapshot row column)))
    (vttest-screen-oracle:cell-rgba
     cell (admitted-glyph-rows font (vttest-screen-oracle:cell-code cell))
     :blink-phase blink-phase :screen-reverse nil
     :cursor (and blink-phase (= row 24) (= column 54)))))

(defun rasterize-snapshot (snapshot font &key font-sha256 (blink-phase nil phase-p))
  "Full opaque 640x384 RGBA. Caller authenticates FONT bytes against FONT-SHA256.
Use the actual closed READY phase. Never search phases or fit expected pixels."
  (admit-raster-input snapshot font font-sha256 blink-phase phase-p)
  (let ((image (make-array (* 640 384 4) :element-type '(unsigned-byte 8)))
        (glyphs (make-hash-table)))
    (dotimes (r 24)
      (dotimes (c 80)
        (let* ((cell (snapshot-cell snapshot (1+ r) (1+ c)))
               (code (vttest-screen-oracle:cell-code cell))
               (rows (or (gethash code glyphs)
                         (setf (gethash code glyphs) (admitted-glyph-rows font code))))
               (tile (vttest-screen-oracle:cell-rgba
                      cell rows :blink-phase blink-phase :screen-reverse nil
                      :cursor (and blink-phase (= r 23) (= c 53)))))
          (dotimes (y 16)
            (replace image tile :start1 (* 4 (+ (* (+ (* r 16) y) 640) (* c 8)))
                                :start2 (* y 32) :end2 (* (1+ y) 32))))))
    image))

(defun compare-snapshot (snapshot actual-rgba font
                         &key font-sha256 (blink-phase nil phase-p))
  "One strict full-frame comparison at the supplied observed phase.
All 245760 pixels, including alpha admission. No mask, crop or normalization."
  (admit-raster-input snapshot font font-sha256 blink-phase phase-p)
  (need (and (vectorp actual-rgba) (= (* 640 384 4) (length actual-rgba))
             (every (lambda (x) (typep x '(unsigned-byte 8))) actual-rgba))
        :rgba-geometry :invalid)
  (need (loop for p from 3 below (length actual-rgba) by 4
              always (= 255 (aref actual-rgba p))) :alpha :nonopaque)
  (let* ((expected (rasterize-snapshot snapshot font :font-sha256 font-sha256
                                                   :blink-phase blink-phase))
         (expected-rgb (make-array (* 640 384 3) :element-type '(unsigned-byte 8)))
         (actual-rgb (make-array (* 640 384 3) :element-type '(unsigned-byte 8))))
    (dotimes (p (* 640 384))
      (dotimes (channel 3)
        (setf (aref expected-rgb (+ (* p 3) channel)) (aref expected (+ (* p 4) channel))
              (aref actual-rgb (+ (* p 3) channel)) (aref actual-rgba (+ (* p 4) channel)))))
    (vttest-cursor-oracle:compare-rgb expected-rgb actual-rgb :width 640)))
