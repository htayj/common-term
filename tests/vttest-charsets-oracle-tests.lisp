;;;; Focused complete-profile controls with actual externally authenticated font bytes.
(load (merge-pathnames "vttest-charsets-oracle.lisp" *load-truename*))
(defpackage #:vttest-charsets-oracle-tests
  (:use #:cl)
  (:export #:run-tests))
(in-package #:vttest-charsets-oracle-tests)

(defun run-tests (font font-sha256)
  "First full focused invocation; no product, native runtime or image files."
  (let* ((count 0) (snapshot (vttest-charsets-oracle:character-set-snapshot))
         (codes (make-array '(24 80) :initial-element 32))
         (bolds (make-array '(24 80) :initial-element nil))
         (samples 0) (optional-samples 0) (ignored 0)
         (dec '(32 9670 9618 9225 9228 9229 9226 176 177 9252 9227
                9496 9488 9484 9492 9532 9146 9147 9472 9148 9149
                9500 9508 9524 9516 9474 8804 8805 960 8800 163 183)))
    (labels ((ok (value)
               (incf count) (unless value (error "Character-set control ~d failed" count)))
             (rejects (thunk &optional field)
               (let ((caught nil))
                 (handler-case (funcall thunk)
                   (vttest-charsets-oracle:invalid-input (condition)
                     (setf caught t)
                     (when field
                       (ok (eq field (vttest-charsets-oracle:invalid-input-field condition))))))
                 (ok caught)))
             (cell (row col) (vttest-charsets-oracle:snapshot-cell snapshot row col))
             (code (row col) (vttest-screen-oracle:cell-code (cell row col)))
             (expected-code (set byte)
               (cond ((and (char= set #\A) (= byte 35)) 163)
                     ((and (char= set #\0) (>= byte 95)) (nth (- byte 95) dec))
                     (t byte)))
             (text (row col string &optional bold)
               (loop for ch across string for c from col do
                 (setf (aref codes (1- row) (1- c)) (char-code ch)
                       (aref bolds (1- row) (1- c)) bold)))
             (raster (&rest arguments)
               (apply #'vttest-charsets-oracle:rasterize-snapshot snapshot font
                      :font-sha256 font-sha256 arguments))
             (compare-image (image)
               (vttest-charsets-oracle:compare-snapshot
                snapshot image font :font-sha256 font-sha256 :blink-phase t))
             (tile (row col phase)
               (vttest-charsets-oracle:resolved-cell-rgba
                snapshot row col font :font-sha256 font-sha256 :blink-phase phase))
             (pixel (image row col x y)
               (let ((p (* 4 (+ (* (+ (* (1- row) 16) y) 640) (* (1- col) 8) x))))
                 (subseq image p (+ p 4))))
             (check-glyph (image row col rows bold cursor)
               ;; Handchecked rows retained by audit352, independent ink arithmetic.
               (dotimes (y 16)
                 (let* ((bits (nth y rows))
                        (ink (if bold (logior bits (ash bits -1)) bits)))
                   (dotimes (x 8)
                     (let ((foreground (logbitp (- 7 x) ink)))
                       (ok (equalp (pixel image row col x y)
                                   (if (if cursor (not foreground) foreground)
                                       #(235 219 178 255) #(29 32 33 255)))))))))
             (copy-region (image dest-row dest-col source source-row source-col)
               (dotimes (y 16)
                 (let ((start (* 4 (+ (* (+ (* (1- dest-row) 16) y) 640)
                                      (* (1- dest-col) 8))))
                       (from (* 4 (+ (* (+ (* (1- source-row) 16) y) 640)
                                     (* (1- source-col) 8)))))
                   (replace image source :start1 start :end1 (+ start 32)
                                         :start2 from :end2 (+ from 32))))))
      (ok (= 24 (vttest-charsets-oracle:snapshot-rows snapshot)))
      (ok (= 80 (vttest-charsets-oracle:snapshot-columns snapshot)))
      (ok (= 1 (vttest-charsets-oracle:snapshot-pause snapshot)))
      (ok (eq snapshot (vttest-charsets-oracle:require-complete snapshot)))
      (ok (= 1920 (length (vttest-charsets-oracle:snapshot-cells snapshot))))
      ;; Independent expectations, not MAP-BYTE used as its own reference.
      (loop for set across "BA012" do
        (loop for byte from 32 to 126 do
          (multiple-value-bind (value status) (vttest-charsets-oracle:map-byte set byte)
            (ok (= value (expected-code set byte))) (ok (eq status :resolved))))
        (loop repeat 2 do
          (multiple-value-bind (value status) (vttest-charsets-oracle:map-byte set 127)
            (incf ignored) (ok (null value)) (ok (eq status :ignored)))))
      (ok (= 10 ignored))
      (ok (= 35 (vttest-charsets-oracle:map-byte #\1 35)))
      (ok (= 113 (vttest-charsets-oracle:map-byte #\2 113)))
      (ok (= 9472 (vttest-charsets-oracle:map-byte #\0 113)))
      (ok (= 163 (vttest-charsets-oracle:map-byte #\A 35)))
      ;; Three explicit chunks per bank, independent of factory's floor formula.
      (text 1 10 "Selected as G0 (with SI)")
      (text 1 48 "Selected as G1 (with SO)")
      (loop for base in '(3 7 11 15 19) for set across "BA012" for label in
        '("Character set B (US ASCII)" "Character set A (British)"
          "Character set 0 (DEC Special graphics and line drawing)"
          "Character set 1 (DEC Alternate character ROM standard characters)"
          "Character set 2 (DEC Alternate character ROM special graphics)") do
        (text base 1 label t)
        (loop for start-column in '(10 48) do
          (loop for row from (1+ base) to (+ base 3)
                for first-byte in '(32 64 96) for size in '(32 32 31) do
            (loop for offset below size for byte from first-byte do
              (incf samples)
              (when (find set "12") (incf optional-samples))
              (setf (aref codes (1- row) (+ (1- start-column) offset))
                    (expected-code set byte))))))
      (text 24 1 "These are the installed character sets. Push <RETURN>")
      (ok (= 950 samples)) (ok (= 380 optional-samples))
      ;; Every cell, ordinary blank and bold label space included.
      (dotimes (r 24)
        (dotimes (c 80)
          (let ((value (cell (1+ r) (1+ c))))
            (ok (= (aref codes r c) (vttest-screen-oracle:cell-code value)))
            (ok (eq (aref bolds r c) (vttest-screen-oracle:cell-bold value)))
            (ok (not (vttest-screen-oracle:cell-inverse value)))
            (ok (not (vttest-screen-oracle:cell-blink value)))
            (ok (not (vttest-screen-oracle:cell-underline value)))
            (ok (= 1 (vttest-screen-oracle:cell-width value)))
            (ok (eq :single (vttest-screen-oracle:cell-line-size value)))
            (ok (eq :default (vttest-screen-oracle:cell-foreground value)))
            (ok (eq :default (vttest-screen-oracle:cell-background value))))))
      (dolist (row '(6 10 14 18 22))
        (dolist (col '(41 79 80)) (ok (= 32 (code row col)))))
      ;; Source footer40 + holdit13 leaves the next cursor at54, not inherited53.
      (ok (= 40 (length "These are the installed character sets. ")))
      (ok (= 13 (length "Push <RETURN>")))
      (ok (= 62 (code 24 53)))
      (ok (= 32 (code 24 54)))
      (let ((metadata (vttest-charsets-oracle:snapshot-metadata snapshot)))
        (ok (equal metadata
          '(:status :complete-reference :area "character-sets" :label "charsets"
            :path (3) :pause 1 :rows 24 :columns 80 :width 640 :height 384
            :cursor (:row 24 :column 54 :visible t)
            :screen-reverse nil :pending-wrap nil :origin nil :margins (1 24)
            :current-pen (:bold nil :underline nil :blink nil :inverse nil
                          :foreground :default :background :default)
            :current-charset (:g0 :ascii :g1 :ascii :invoked :g0)
            :alternate-rom-policy :matches-xterm-ascii
            :xterm-commit "5c75ead9a75d4b2e666ca06fa94503b5766ed93f"
            :source-charsets-sha256 "0b33ebeb34382620e526e470f8ecf90cc5e779bc604187ab957ca7a0fd0fa0e9"
            :printable-samples 950 :ignored-del 10
            :requirements (:vt100 :seven-bit :normal-sgr :normal-screen :80x24
                           :single-lines :full-margins :absolute-origin
                           :highlighting-disabled :observed-boolean-blink-phase))))
        (let ((original (vttest-charsets-oracle:snapshot-metadata snapshot))
              (copy (vttest-charsets-oracle:snapshot-cells snapshot)))
          (setf (aref copy 0) nil (getf (getf metadata :cursor) :row) 99
                (first (getf metadata :path)) 99 (char (getf metadata :area) 0) #\X)
          (ok (equal original (vttest-charsets-oracle:snapshot-metadata snapshot)))
          (ok (= 32 (code 1 1)))))
      (rejects (lambda () (cell 0 1)) :row)
      (rejects (lambda () (cell 1 81)) :column)
      (dolist (byte '(31 128 -1 nil))
        (rejects (lambda () (vttest-charsets-oracle:map-byte #\B byte)) :byte))
      (rejects (lambda () (vttest-charsets-oracle:map-byte #\Z 65)) :designator)
      (rejects (lambda () (vttest-charsets-oracle:require-complete
                          (vttest-charsets-oracle::%snapshot #(nil)))) :cell-count)
      (let ((bad (make-array 1920 :initial-element nil)))
        (rejects (lambda () (vttest-charsets-oracle:require-complete
                            (vttest-charsets-oracle::%snapshot bad))) :cell))
      (rejects (lambda () (raster)) :blink-phase)
      (rejects (lambda () (raster :blink-phase 1)) :blink-phase)
      (rejects (lambda () (vttest-charsets-oracle:rasterize-snapshot
                          snapshot font :blink-phase t :font-sha256 "wrong")) :font-sha256)
      (rejects (lambda () (vttest-charsets-oracle:rasterize-snapshot
                          snapshot font :blink-phase t)) :font-sha256)
      (rejects (lambda () (vttest-charsets-oracle:rasterize-snapshot
                          snapshot #(0) :font-sha256 font-sha256 :blink-phase t)) :font-layout)
      (dolist (code '(#x378 #xffff #x10000 -1 #xd800))
        (rejects (lambda () (vttest-charsets-oracle:admitted-glyph-rows font code))))
      ;; Small explicit bitmap, with clipped bold neighbor and no row leakage.
      (let* ((rows #(128 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
             (normal (vttest-screen-oracle:cell-rgba
                      (vttest-screen-oracle:make-cell) rows :blink-phase nil))
             (bold (vttest-screen-oracle:cell-rgba
                    (vttest-screen-oracle:make-cell :bold t) rows :blink-phase nil)))
        (ok (equalp #(235 219 178 255) (subseq normal 0 4)))
        (ok (equalp #(29 32 33 255) (subseq normal 4 8)))
        (ok (equalp #(235 219 178 255) (subseq bold 4 8)))
        (ok (equalp #(235 219 178 255) (subseq bold 60 64)))
        (ok (equalp #(29 32 33 255) (subseq bold 64 68))))
      ;; Full actual-font success. Test phase is explicit T, not claimed native READY.
      (let* ((image (raster :blink-phase t)) (comparison (compare-image image)))
        (ok (= (* 640 384 4) (length image)))
        (ok (loop for p from 3 below (length image) by 4 always (= 255 (aref image p))))
        (ok (getf comparison :pass))
        (ok (= 245760 (getf comparison :compared-pixels)))
        (ok (= 0 (getf comparison :different-pixels)))
        (ok (= 0 (getf comparison :masked-pixels)))
        (ok (= 0 (getf comparison :tolerance)))
        (dolist (col '(13 51))
          (check-glyph image 4 col '(0 0 36 36 36 126 36 36 126 36 36 36 0 0 0 0) nil nil)
          (check-glyph image 8 col '(0 0 24 36 32 32 120 32 32 32 34 126 0 0 0 0) nil nil))
        (check-glyph image 14 10 '(0 0 0 0 16 56 124 254 124 56 16 0 0 0 0 0) nil nil)
        (check-glyph image 3 1 '(0 0 60 66 66 64 64 64 64 66 66 60 0 0 0 0) t nil)
        (check-glyph image 24 53 '(0 0 0 64 32 16 8 4 8 16 32 64 0 0 0 0) nil nil)
        (check-glyph image 24 54 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) nil t)
        (check-glyph image 22 79 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) nil nil)
        (let ((off (tile 24 54 nil)) (on (tile 24 54 t)))
          (ok (not (equalp off on)))
          (dotimes (p 128)
            (ok (equalp #(29 32 33 255) (subseq off (* p 4) (+ (* p 4) 4))))
            (ok (equalp #(235 219 178 255) (subseq on (* p 4) (+ (* p 4) 4))))))
        (let ((bad (copy-seq image)))
          (setf (aref bad 0) (logxor 1 (aref bad 0)))
          (let ((result (compare-image bad)))
            (ok (not (getf result :pass))) (ok (= 1 (getf result :different-pixels)))))
        (let ((bad (copy-seq image)))
          (setf (aref bad 3) 254)
          (rejects (lambda () (compare-image bad)) :alpha))
        (rejects (lambda () (compare-image (subseq image 0 (1- (length image))))) :rgba-geometry)
        (let ((bad (copy-seq image)))
          ;; Omit set2/G1's three complete sample rows.
          (loop for row from 20 to 22 do
            (loop for col from 48 to 79 do (copy-region bad row col image 2 1)))
          (ok (not (getf (compare-image bad) :pass))))
        (let ((bad (copy-seq image)))
          ;; Erroneously map set2/G1 to DEC0 rather than ASCII.
          (loop for row from 20 to 22 for source-row from 12 do
            (loop for col from 48 to 79 do (copy-region bad row col image source-row col)))
          (ok (not (getf (compare-image bad) :pass)))))
      (format t "Character-set controls: ~d PASS, 0 failures;1920 cells/950 samples/10 DEL/380 resolved alternate samples;actual-font full opaque640x384 exact comparison PASS~%" count)
      (list :passed count :failed 0 :cells 1920 :printable-samples samples :ignored-del ignored
            :resolved-alternate-samples optional-samples :full-reference :tested
            :actual-font t :native-acceptance :not-run))))
