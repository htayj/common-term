;;;; Pure, conditional preparation for vttest 20251205 screen pauses 1..15.
;;;; Declarative cells from reviewed main.c, not a product parser or grid.
(load (merge-pathnames "vttest-cursor-oracle.lisp" *load-truename*))

(defpackage #:vttest-screen-oracle
  (:use #:cl)
  (:export #:profile-error #:profile-error-field #:profile-error-value
           #:make-cell #:cell-code #:cell-width #:cell-foreground #:cell-background
           #:cell-bold #:cell-underline #:cell-blink #:cell-inverse #:cell-line-size
           #:screen-snapshot #:snapshot-pause #:snapshot-rows #:snapshot-columns
           #:snapshot-cell #:snapshot-cells #:snapshot-metadata #:compare-snapshot
           #:cell-rgba #:rasterize-attributed #:compare-screen-snapshot))
(in-package #:vttest-screen-oracle)

(define-condition profile-error (error)
  ((field :initarg :field :reader profile-error-field)
   (value :initarg :value :reader profile-error-value))
  (:report (lambda (condition stream)
             (format stream "Outside attributed screen profile: ~s = ~s"
                     (profile-error-field condition) (profile-error-value condition)))))

(defun need (predicate field value)
  (unless predicate (error 'profile-error :field field :value value)))

(defun boolean-p (value) (or (eq value nil) (eq value t)))

(defstruct (cell (:constructor %make-cell) (:copier nil))
  (code 32 :read-only t) (width 1 :read-only t)
  (foreground :default :read-only t) (background :default :read-only t)
  (bold nil :read-only t) (underline nil :read-only t)
  (blink nil :read-only t) (inverse nil :read-only t)
  (line-size :single :read-only t))

(defun make-cell (&key (code 32) (width 1) (foreground :default) (background :default)
                       bold underline blink inverse (line-size :single))
  "An immutable single-width default-color cell. No coercion of attributes."
  (need (and (integerp code) (<= 32 code #xffff)
             (not (<= #xd800 code #xdfff)) (code-char code)) :code code)
  (need (eql width 1) :width width)
  (need (eq foreground :default) :foreground foreground)
  (need (eq background :default) :background background)
  (need (eq line-size :single) :line-size line-size)
  (loop for field in '(:bold :underline :blink :inverse)
        for flag in (list bold underline blink inverse)
        do (need (boolean-p flag) field flag))
  (%make-cell :code code :width width :foreground foreground :background background
              :bold bold :underline underline :blink blink :inverse inverse :line-size line-size))

(defstruct (snapshot (:constructor %make-snapshot) (:copier nil)
                     (:conc-name %snapshot-))
  (pause nil :read-only t) (cells nil :read-only t) (metadata nil :read-only t))

(defun snapshot-pause (snapshot) (%snapshot-pause snapshot))
(defun snapshot-rows (snapshot) (check-type snapshot snapshot) 24)
(defun snapshot-columns (snapshot)
  (check-type snapshot snapshot)
  (/ (length (%snapshot-cells snapshot)) 24))
(defun snapshot-cells (snapshot)
  "Fresh complete row-major vector of immutable cells, never the backing vector."
  (copy-seq (%snapshot-cells snapshot)))
(defun snapshot-metadata (snapshot)
  "Fresh metadata tree, including strings. Saved slot at 13/14 is unbound."
  (labels ((copy-value (value)
             (typecase value
               (cons (cons (copy-value (car value)) (copy-value (cdr value))))
               (string (copy-seq value))
               (t value))))
    (copy-value (%snapshot-metadata snapshot))))
(defun snapshot-cell (snapshot row column)
  "Immutable cell at one-based ROW,COLUMN."
  (need (and (integerp row) (<= 1 row 24)) :row row)
  (need (and (integerp column) (<= 1 column (snapshot-columns snapshot))) :column column)
  (aref (%snapshot-cells snapshot) (+ (* (1- row) (snapshot-columns snapshot)) (1- column))))

(defun normal-snapshot (pause)
  "Conditional full-grid contracts for main.c621..660, not runtime admission."
  (let ((cells (make-array 1920 :initial-element (make-cell))))
    (labels ((text (row column string)
               (loop for ch across string for col from column do
                 (setf (aref cells (+ (* (1- row) 80) (1- col)))
                       (make-cell :code (char-code ch))))))
      (ecase pause
        (1
         (loop for row from 1 to 3 do (text row 1 (make-string 80 :initial-element #\*)))
         (text 5 1 "This should be three identical lines of *'s completely filling")
         (text 6 1 "the top of the screen without any empty lines between.")
         (text 7 1 "(Test of WRAP AROUND mode setting.)")
         (text 8 1 "Push <RETURN>"))
        (2
         (dolist (row '(1 2))
           (loop for column from 7 to 79 by 6 do (text row column "*")))
         (text 4 1 "Test of TAB setting/resetting. These two lines")
         (text 5 1 "should look the same. Push <RETURN>"))))
    (%make-snapshot
     :pause pause :cells cells
     :metadata
     (list :cursor (list :row (if (= pause 1) 8 5)
                         :column (if (= pause 1) 14 36) :visible t)
           :screen-reverse nil :blink-visible nil
           :current-pen '(:bold nil :underline nil :blink nil :inverse nil
                          :foreground :default :background :default)
           :current-charset '(:g0 :ascii :g1 :inherited-unbound :invoked :g0)
           :pending-wrap nil :origin nil :margins '(1 24) :autowrap t
           :insert :inherited-unbound :saved-slot :inherited-unbound
           ;; Visible projection only. No claim about hidden/offscreen tab retention.
           :visible-tabs (if (= pause 1) '(9 17 25 33 41 49 57 65 73)
                             '(7 13 19 25 31 37 43 49 55 61 67 73 79))
           :source-main-sha256 "4bd1d88d41c2cc346ae9ec858f11d87243eafbb829b4a0bf40168c45a364c3c1"
           :source-main-lines (if (= pause 1) '(621 634) '(636 660))
           :requirements
           (list :status :conditional
                 :entry '(:80x24 :single-lines :normal-sgr :ascii-g0-gl :full-margins
                          :absolute-origin :normal-screen :lnm-reset :default-visible-tabs)
                 :predecessor (if (= pause 1) :fresh-area-cleared
                                  :pause1-then-return-before-explicit-ed2)
                 :output :complete-source-wire-lf-to-crlf-preserve-other-octets
                 :boundary :post-prompt-before-reply-or-extraneous-output
                 :terminal '(:delayed-wrap :positioning-cancels-wrap :cuf-clamp
                             :ht-next-or-margin :tbc1-and2-ignored)
                 :cursor :externally-admitted-visible-steady)))))

(defun width-background-snapshot (pause columns)
  "Conditional main.c661..693 cells. Visible tabs do not bind hidden retention."
  (let* ((light (member pause '(3 4)))
         (caption (format nil "This is ~d column mode, ~a background."
                          columns (if light "light" "dark")))
         (cells (make-array (* 24 columns) :initial-element (make-cell))))
    (labels ((text (row column string)
               (loop for ch across string for col from column do
                 (setf (aref cells (+ (* (1- row) columns) (1- col)))
                       (make-cell :code (char-code ch))))))
      ;; Precision W-col in the source leaves the physical last column blank.
      (loop for col from 1 below columns do
        (text 1 col (string (char "1234567890" (mod (1- col) 10)))))
      (loop for row from 3 to 20 do (text row row caption))
      (text 20 (+ 20 (length caption)) "Push <RETURN>"))
    (%make-snapshot
     :pause pause :cells cells
     :metadata
     (list :cursor (list :row 20 :column (+ 33 (length caption)) :visible t)
           :screen-reverse (not (null light)) :blink-visible nil
           :current-pen '(:bold nil :underline nil :blink nil :inverse nil
                          :foreground :default :background :default)
           :current-charset '(:g0 :ascii :g1 :inherited-unbound :invoked :g0)
           :pending-wrap nil :origin nil :margins '(1 24) :autowrap t
           :insert :inherited-unbound :saved-slot :inherited-unbound
           :column-mode columns :lnm nil :smooth-scroll nil
           :visible-tabs (append (loop for col from 9 below columns by 8 collect col)
                                 (when (= columns 132) '(132)))
           :source-main-sha256 "4bd1d88d41c2cc346ae9ec858f11d87243eafbb829b4a0bf40168c45a364c3c1"
           :source-main-lines (if (= columns 132) '(661 682) '(683 693))
           :source-members
           '((:member "vttest-20251205/main.c" :lines (661 693))
             (:member "vttest-20251205/esc.c" :lines (164 184)
              :sha256 "de36d3e70cf0d750ab9a444af9419b0fc45f08415430474e66b5e54a687a645d")
             (:member "vttest-20251205/unix_io.c" :lines (237 270)
              :sha256 "a92b41cc23ae55fdd35a36818a4ec1a69e9f0d2c6606d1dbb3f1e6f732f2ab73"))
           :requirements
           (list :status :conditional
                 :entry '(:fresh-area-80x24 :single-lines :normal-sgr :ascii-g0-gl
                          :full-margins :absolute-origin :autowrap :lnm-reset :jump-scroll)
                 :predecessor (list :pause (1- pause) :reply :return
                                    :then (if (= columns 132)
                                              :explicit-background-deccolm132-ed2-cup-tbc3-hts
                                              :deccolm80-ed2-cup))
                 :dimensions (list 24 columns 8 16)
                 :output :complete-source-wire-lf-to-crlf-preserve-other-octets
                 :input-echo-profile :inherited-canonical-echo-crlf-v1
                 :boundary :post-prompt-before-reply-or-extraneous-output
                 :terminal '(:deccolm-exact-width :single-lines :ed2-normal-spaces
                             :positioning-cancels-wrap :cuf-clamp :global-screen-reversal)
                 :tabs :visible-projection-only
                 :cursor :externally-admitted-visible-steady)))))

(defun middle-snapshot (pause)
  "Completed main.c549..591/695 scrolling and 696..711 origin holds.
No intermediate upward-scroll image is a hold boundary. All modes conditional."
  (let* ((scroll (<= pause 10)) (full (member pause '(8 10)))
         (top (if scroll (if full 1 12) (if (= pause 11) 23 1)))
         (bottom (if scroll (if full 24 13) 24))
         (smooth (not (null (member pause '(7 8)))))
         (cells (make-array 1920 :initial-element (make-cell))))
    (labels ((text (row string)
               (loop for ch across string for col from 0 do
                 (setf (aref cells (+ (* (1- row) 80) col))
                       (make-cell :code (char-code ch))))))
      (if scroll
          (progn
            ;; LF then TWO RI per down iteration inserts a blank top row;
            ;; after 29 iterations the lower rows contain D29..D7 or D29.
            (loop for row from (1+ top) to bottom do
              (text row (format nil "~a scroll down region [~d..~d] size ~d Line ~d"
                                (if smooth "Soft" "Jump") top bottom
                                (1+ (- bottom top)) (- 30 (- row top)))))
            (text top "Push <RETURN>"))
          (progn
            (text 24 "Origin mode test. This line should be at the bottom of the screen.")
            (text top (concatenate 'string
                         (if (= pause 11)
                             "This line should be the one above the bottom of the screen. "
                             "This line should be at the top of the screen. ")
                         "Push <RETURN>")))))
    (%make-snapshot
     :pause pause :cells cells
     :metadata
     (list :cursor (list :row top :column (if scroll 14 (if (= pause 11) 74 60)) :visible t)
           :screen-reverse nil :blink-visible nil
           :current-pen '(:bold nil :underline nil :blink nil :inverse nil
                          :foreground :default :background :default)
           :current-charset '(:g0 :ascii :g1 :inherited-unbound :invoked :g0)
           :pending-wrap nil :origin (/= pause 12)
           :margins (if scroll (list top bottom) '(23 24)) :autowrap t
           :insert :inherited-unbound :saved-slot :inherited-unbound
           :column-mode 80 :lnm nil :smooth-scroll smooth
           :visible-tabs '(9 17 25 33 41 49 57 65 73)
           :source-main-sha256 "4bd1d88d41c2cc346ae9ec858f11d87243eafbb829b4a0bf40168c45a364c3c1"
           :source-main-lines (cond (scroll '(549 591 695)) ((= pause 11) '(696 703))
                                    (t '(704 711)))
           :requirements
           (list :status :conditional :predecessor (list :pause (1- pause) :reply :return)
                 :entry '(:source-sequence-from-fresh-area :single-lines :normal-sgr
                          :ascii-g0-gl :autowrap :lnm-reset :80x24)
                 :output :complete-source-wire-lf-to-crlf-preserve-other-octets
                 :input-echo-profile :inherited-canonical-echo-crlf-v1
                 :boundary :post-prompt-before-reply-or-extraneous-output
                 :terminal '(:ed2-whole-display :decom-relative-cup :margin-clamped-cuu-cud
                             :two-ri-per-down-iteration :positioning-cancels-wrap)
                 :tabs :visible-projection-only
                 :cursor :externally-admitted-visible-steady)))))

(defun screen-snapshot (pause &key (rows 24) (columns 80) (cell-width 8) (cell-height 16))
  "Declarative snapshot, independent of phase and terminal state.
This preparation is conditional on later entry/output-state admission.
Only pauses 1..15 are accepted, at 24 rows and 8x16 cells.
Columns default to 80; pauses 3/5 require explicit :columns 132."
  (need (and (integerp pause) (<= 1 pause 15)) :pause pause)
  (need (equal (list rows columns cell-width cell-height)
               (list 24 (if (member pause '(3 5)) 132 80) 8 16))
        :dimensions (list rows columns cell-width cell-height))
  (when (member pause '(3 4 5 6))
    (return-from screen-snapshot (width-background-snapshot pause columns)))
  (when (member pause '(1 2)) (return-from screen-snapshot (normal-snapshot pause)))
  (when (<= 7 pause 12) (return-from screen-snapshot (middle-snapshot pause)))
  (let ((cells (make-array 1920 :initial-element (make-cell))))
    (labels ((text (row column string &optional (flags 0))
               ;; Declarative flags: B=1, U=2, K=4, I=8. No SGR interpreter.
               (loop for ch across string for col from column do
                 (need (<= col 80) :span-end col)
                 (setf (aref cells (+ (* (1- row) 80) (1- col)))
                       (make-cell :code (char-code ch) :bold (logbitp 0 flags)
                                  :underline (logbitp 1 flags) :blink (logbitp 2 flags)
                                  :inverse (logbitp 3 flags))))))
      (if (< pause 15)
          (progn
            (text 1 20 "Graphic rendition test pattern:")
            (dolist (span '((4 1 "vanilla" 0) (4 40 "bold" 1)
                            (6 6 "underline" 2) (6 45 "bold underline" 3)
                            (8 1 "blink" 4) (8 40 "bold blink" 5)
                            (10 6 "underline blink" 6) (10 45 "bold underline blink" 7)
                            (12 1 "negative" 8) (12 40 "bold negative" 9)
                            (14 6 "underline negative" 10) (14 45 "bold underline negative" 11)
                            (16 1 "blink negative" 12) (16 40 "bold blink negative" 13)
                            (18 6 "underline blink negative" 14)
                            (18 45 "bold underline blink negative" 15)))
              (apply #'text span))
            (text 23 1 (if (= pause 13) "Dark background. Push <RETURN>"
                          "Light background. Push <RETURN>")))
          (progn
            (dolist (span '((8 12 "normal") (8 24 "bold") (8 36 "underscored")
                            (8 48 "blinking") (8 60 "reversed") (10 1 "stars:")
                            (12 1 "line:") (14 1 "x'es:") (16 1 "diamonds:")
                            (21 1 "Test of the SAVE/RESTORE CURSOR feature. There should")
                            (22 1 "be ten characters of each flavour, and a rectangle")
                            (23 1 "of 5 x 4 A's filling the top left of the screen.")
                            (24 1 "Push <RETURN>")))
              (apply #'text span))
            (loop for row from 1 to 4 do (text row 1 "AAAAA"))
            (loop for row in '(10 12 14 16) for code in '(42 #x2500 120 #x25c6) do
              (loop for column in '(12 24 36 48 60) for flags in '(0 1 2 4 8) do
                (text row column (make-string 10 :initial-element (code-char code)) flags)))))
      (%make-snapshot
       :pause pause :cells cells
       :metadata (list :cursor (list :row (if (= pause 15) 24 23)
                                    :column (ecase pause (13 31) (14 32) (15 14)) :visible t)
                       :screen-reverse (= pause 14) :blink-visible t
                       :current-pen '(:bold nil :underline nil :blink nil :inverse nil
                                      :foreground :default :background :default)
                       :current-charset '(:g0 :ascii :g1 :ascii :invoked :g0)
                       :pending-wrap nil :origin nil :margins '(1 24)
                       :autowrap t :insert nil
                       :saved-slot (if (= pause 15)
                                       '(:row 16 :column 65 :pen (:bold nil :underline nil :blink nil
                                                               :inverse t :foreground :default :background :default)
                                         :charset (:g0 :graphics :g1 :ascii :invoked :g0)
                                         :origin nil :pending-wrap nil)
                                       :inherited-unbound)
                       :source-main-sha256 "4bd1d88d41c2cc346ae9ec858f11d87243eafbb829b4a0bf40168c45a364c3c1")))))

(defun cell-equal-p (a b)
  (and (cell-p a) (cell-p b)
       (every (lambda (reader) (eql (funcall reader a) (funcall reader b)))
              (list #'cell-code #'cell-width #'cell-foreground #'cell-background
                    #'cell-bold #'cell-underline #'cell-blink #'cell-inverse #'cell-line-size))))

(defun compare-snapshot (snapshot cells metadata)
  "Compare a complete row-major semantic reference and metadata, without pixels."
  (need (and (vectorp cells) (= (length (%snapshot-cells snapshot)) (length cells)))
        :cell-count (type-of cells))
  (let ((different (loop for a across (%snapshot-cells snapshot) for b across cells
                         count (not (cell-equal-p a b)))))
    (list :pass (and (zerop different) (equal (snapshot-metadata snapshot) metadata))
          :compared-cells (length (%snapshot-cells snapshot)) :different-cells different
          :metadata-equal (equal (snapshot-metadata snapshot) metadata))))

(defun cell-rgba (cell glyph-rows &key (blink-phase nil phase-p)
                                       (screen-reverse nil) (cursor nil))
  "Pure 8x16 RGBA cell from sixteen explicit bitmap bytes. Phase is mandatory.
Bold is a clipped right-neighbor union; underline y14; inversion is XOR.
Blink gates foreground only. All pixels are opaque. No font or product state."
  (need (cell-p cell) :cell cell)
  (need (and phase-p (boolean-p blink-phase)) :blink-phase blink-phase)
  (need (boolean-p screen-reverse) :screen-reverse screen-reverse)
  (need (boolean-p cursor) :cursor cursor)
  (need (and (vectorp glyph-rows) (= 16 (length glyph-rows))
             (every (lambda (byte) (typep byte '(unsigned-byte 8))) glyph-rows))
        :glyph-rows glyph-rows)
  (let* ((rgba (make-array 512 :element-type '(unsigned-byte 8)))
         (inverted (oddp (count t (list (cell-inverse cell) screen-reverse cursor))))
         (fg (if inverted '(29 32 33) '(235 219 178)))
         (bg (if inverted '(235 219 178) '(29 32 33))))
    (dotimes (y 16)
      (dotimes (x 8)
        (let* ((ink (and (or (not (cell-blink cell)) blink-phase)
                         (or (logbitp (- 7 x) (aref glyph-rows y))
                             (and (cell-bold cell) (> x 0)
                                  (logbitp (- 8 x) (aref glyph-rows y)))
                             (and (cell-underline cell) (= y 14)))))
               (p (* 4 (+ (* y 8) x))))
          (loop for value in (if ink fg bg) for i from p do (setf (aref rgba i) value))
          (setf (aref rgba (+ p 3)) 255))))
    rgba))

(defun rasterize-attributed (snapshot font-octets &key (blink-phase nil phase-p))
  "Exact 8x16-cell RGBA, phase explicitly supplied, never selected by pixel fit.
Caller must freshly verify the canonical regular Terminus font digest
0d11d8ed0889a412fb6903710804643ad85858cc56905286cedae392a57255ca
before reading FONT-OCTETS. Only the reviewed pure decoder is called."
  (need (and phase-p (boolean-p blink-phase)) :blink-phase blink-phase)
  (check-type snapshot snapshot)
  (vttest-cursor-oracle:font-info font-octets)
  (let* ((width (* 8 (snapshot-columns snapshot)))
         (image (make-array (* width 384 4) :element-type '(unsigned-byte 8)))
         (metadata (snapshot-metadata snapshot)) (cursor (getf metadata :cursor))
         (glyphs (make-hash-table)))
    (dotimes (r 24)
      (dotimes (c (snapshot-columns snapshot))
        (let* ((cell (snapshot-cell snapshot (1+ r) (1+ c))) (code (cell-code cell))
               (rows (or (gethash code glyphs)
                         (setf (gethash code glyphs)
                               (vttest-cursor-oracle:glyph-rows font-octets (code-char code)))))
               (tile (cell-rgba cell rows :blink-phase blink-phase
                               :screen-reverse (getf metadata :screen-reverse)
                               :cursor (and (getf cursor :visible) (= r (1- (getf cursor :row)))
                                            (= c (1- (getf cursor :column)))))))
          (dotimes (y 16)
            (replace image tile :start1 (* 4 (+ (* (+ (* r 16) y) width) (* c 8)))
                                :start2 (* y 32) :end2 (* (1+ y) 32))))))
    image))

(defun compare-screen-snapshot (snapshot actual-rgba font-octets &key (blink-phase nil phase-p))
  "Full-image exact comparison against the one explicitly admitted phase.
Reject nonopaque alpha; no files, tolerances, masks or alternative phases.
This does not admit a phase or validate native capture custody."
  (need (and phase-p (boolean-p blink-phase)) :blink-phase blink-phase)
  (need (and (vectorp actual-rgba)
             (= (* (snapshot-columns snapshot) 8 384 4) (length actual-rgba))
             (every (lambda (byte) (typep byte '(unsigned-byte 8))) actual-rgba))
        :rgba-profile (type-of actual-rgba))
  (need (loop for p from 3 below (length actual-rgba) by 4 always (= 255 (aref actual-rgba p)))
        :alpha :nonopaque)
  (let* ((expected (rasterize-attributed snapshot font-octets :blink-phase blink-phase))
         (rgb-size (* (snapshot-columns snapshot) 8 384 3))
         (expected-rgb (make-array rgb-size :element-type '(unsigned-byte 8)))
         (actual-rgb (make-array rgb-size :element-type '(unsigned-byte 8))))
    (dotimes (p (* (snapshot-columns snapshot) 8 384))
      (dotimes (channel 3)
        (setf (aref expected-rgb (+ (* p 3) channel)) (aref expected (+ (* p 4) channel))
              (aref actual-rgb (+ (* p 3) channel)) (aref actual-rgba (+ (* p 4) channel)))))
    (vttest-cursor-oracle:compare-rgb expected-rgb actual-rgb
                                      :width (* 8 (snapshot-columns snapshot)))))
