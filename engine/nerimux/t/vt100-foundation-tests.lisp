(in-package #:nerimux/test/terminal)

;; Standards: https://vt100.net/docs/vt100-ug/chapter3.html
;; C0: S3.3.1; margins: CUU/CUD/IND/RI/DECOM; tab stops: TBC;
;; charset: SCS; reports: DECREQTPARM/DECREPTPARM.
;; Outside-margin directional bounds match xterm cursor.c CursorUp/CursorDown:
;; https://github.com/ThomasDickey/xterm-snapshots/blob/master/cursor.c

(defun foundation-feed-octets (screen bytes)
  "Exercise CPS continuation boundaries by feeding each octet separately."
  (let ((one (make-array 1 :element-type '(unsigned-byte 8))))
    (map nil (lambda (byte)
               (setf (aref one 0) byte)
               (screen-process-bytes screen one))
         bytes))
  screen)

(defun foundation-bounds-p (screen)
  (and (<= 0 (screen-cursor-x screen) (1- (screen-width screen)))
       (<= 0 (screen-cursor-y screen) (1- (screen-height screen)))
       (<= 0 (nerimux/terminal/types:screen-scroll-top screen)
           (nerimux/terminal/types:screen-scroll-bottom screen)
           (1- (screen-height screen)))))

(describe "terminal-suite/vt100-foundation-margins"
  (it "replays the exact 17-octet physical-top RI crash"
    (with-screen (s 80 24)
      (foundation-feed-octets s #(27 91 49 50 59 49 51 114 27 91 49 59 49 72 27 77 83))
      (check-cursor s 1 0)
      (expect (char= #\S (char-at s 0 0)))
      (expect (foundation-bounds-p s))))

  (it "indexes in either direction only at the matching margin"
    ;; Geometry 8x7, region rows 2..4; include physical edges and both outsiders.
    (dolist (case '((0 0 1) (1 0 2) (2 2 3) (3 2 4) (4 3 4) (5 4 6) (6 5 6)))
      (destructuring-bind (start ri lf) case
        (dolist (movement (list (list (esc "M") ri) (list (esc "D") lf)
                               (list (string #\Linefeed) lf)))
          (with-screen (s 8 7)
            (feed s (csi "3;5" #\r))
            (feed s (csi (format nil "~D;4" (1+ start)) #\H))
            (dotimes (y 7)
              (setf (nerimux/terminal/types:screen-cell s 0 y)
                    (nerimux/terminal/types:make-cell :char (digit-char y))))
            (feed s (first movement))
            (check-cursor s 3 (second movement))
            (expect (foundation-bounds-p s))
            ;; Neither outside edge may scroll the region.
            (when (or (< start 2) (> start 4))
              (dotimes (y 7)
                (expect (char= (digit-char y) (char-at s 0 y))))))))))

  (it "scrolls content exclusively inside the region at its boundaries"
    (dolist (reverse '(nil t))
      (with-screen (s 8 7)
        (feed s (csi "3;5" #\r))
        (dotimes (y 7)
          (setf (nerimux/terminal/types:screen-cell s 0 y)
                (nerimux/terminal/types:make-cell :char (digit-char y))))
        (feed s (csi (if reverse "3;4" "5;4") #\H))
        (feed s (esc (if reverse "M" "D")))
        (expect (char= #\0 (char-at s 0 0)))
        (expect (char= #\1 (char-at s 0 1)))
        (expect (char= #\5 (char-at s 0 5)))
        (expect (char= #\6 (char-at s 0 6)))
        (expect (char= #\Space (char-at s 0 (if reverse 2 4))))
        (expect (char= (if reverse #\2 #\4) (char-at s 0 3))))))

  (it "uses directional margins for CSI A B E F e and preserves column policy"
    (dolist (start '(0 1 2 3 4 5 6))
      (dolist (final '(#\A #\B #\E #\F #\e))
        (with-screen (s 8 7)
          (feed s (csi "3;5" #\r))
          (feed s (csi (format nil "~D;4" (1+ start)) #\H))
          (feed s (csi "99" final))
          (let* ((up (member final '(#\A #\F)))
                 (expected (if up (if (< start 2) 0 2) (if (> start 4) 6 4))))
            (check-cursor s (if (member final '(#\E #\F)) 0 3) expected))
          (expect (foundation-bounds-p s))))))

  (it "clamps origin-relative positioning and movement inside the margins"
    (with-screen (s 8 7)
      (feed s (csi "3;5" #\r))
      (feed s (csi "?6" #\h))
      (feed s (csi "1;4" #\H))
      (check-cursor s 3 2)
      (feed s (csi "99" #\A))
      (check-cursor s 3 2)
      (feed s (csi "99" #\B))
      (check-cursor s 3 4)
      (feed s (csi "99;99" #\H))
      (check-cursor s 7 4)
      (feed s (csi "?6" #\l))
      (check-cursor s 0 0)))

  (it "handles one-row screens and legal inclusive one-row regions"
    (dolist (height '(1 2 5))
      (dotimes (margin height)
        (with-screen (s 2 height)
          ;; Inclusive margins may be equal; do not confuse a bounds invariant
          ;; with DECSTBM's separate validation of a requested scrolling region.
          (setf (nerimux/terminal/types:screen-scroll-top s) margin
                (nerimux/terminal/types:screen-scroll-bottom s) margin)
          (dotimes (start height)
            (nerimux/terminal/actions:set-cursor s 0 start)
            (feed s (esc "M"))
            (expect (foundation-bounds-p s))
            (feed s (esc "D"))
            (expect (foundation-bounds-p s))
            (feed s "ABC")
            (expect (foundation-bounds-p s)))))))

  (it-fuzz "preserves bounds after each byte in small geometries and inclusive margins"
      ((width (gen-integer :min 1 :max 16))
       (height (gen-integer :min 1 :max 8))
       (a (gen-integer :min 0 :max 255))
       (b (gen-integer :min 0 :max 255))
       (bytes (gen-vector (gen-integer :min 0 :max 255) :min-length 0 :max-length 128)))
      (:trials 400 :timeout-per-trial 2)
    (with-screen (s width height)
      (setf (nerimux/terminal/types:screen-scroll-top s) (min (mod a height) (mod b height))
            (nerimux/terminal/types:screen-scroll-bottom s) (max (mod a height) (mod b height)))
      (map nil (lambda (byte)
                 (foundation-feed-octets s (vector byte))
                 (expect (foundation-bounds-p s)))
           bytes)))

  (it-fuzz "preserves bounds across generated movement streams on 80x24 and 10x5"
      ((ops (gen-vector (gen-integer :min 0 :max 255) :min-length 1 :max-length 100)))
      (:trials 200 :timeout-per-trial 2)
    (dolist (geometry '((80 24) (10 5)))
      (with-screen (s (first geometry) (second geometry))
        (loop for op across ops do
          (feed s (case (mod op 10)
                    (0 (esc "M")) (1 (esc "D"))
                    (2 (csi (format nil "~D" op) #\A))
                    (3 (csi (format nil "~D" op) #\B))
                    (4 (csi (format nil "~D;~D" (mod op 30) (mod op 90)) #\H))
                    (5 (csi (format nil "~D;~D" (mod op 10) (mod op 25)) #\r))
                    (6 (csi "?6" #\h)) (7 (csi "?6" #\l))
                    (8 "abcdefghijk") (t (format nil "~C~C" #\Return #\Linefeed))))
          (expect (foundation-bounds-p s)))))))

(describe "terminal-suite/vt100-foundation-tabs"
  (it "ignores unsupported TBC modes and distinguishes current from all"
    (with-screen (s 40 3)
      (feed s (csi "1;9" #\H))
      (dolist (mode '(1 2 4 99))
        (feed s (csi (write-to-string mode) #\g))
        (expect (eq :default (nerimux/terminal/types:screen-tab-stops s))))
      (feed s (csi "" #\g))
      (expect (equal '(16 24 32) (nerimux/terminal/types:screen-tab-stops s)))
      (feed s (csi "1;17" #\H))
      (feed s (csi "0" #\g))
      (expect (equal '(24 32) (nerimux/terminal/types:screen-tab-stops s)))
      (feed s (csi "3" #\g))
      (expect (null (nerimux/terminal/types:screen-tab-stops s))))))

(describe "terminal-suite/vt100-foundation-embedded-controls"
  (it "reproduces vttest005's four identical lines bytewise"
    ;; vttest 20251205 main.c:508-525, BS inside CUF, CR inside CUF,
    ;; and VT inside CUU. Keep this small fixture independent of .build.
    (with-screen (s 80 24)
      (feed s (format nil "A B C D E F G H I~C~C" #\Return #\Linefeed))
      (loop for i from 1 to 9 do
        (foundation-feed-octets s (octets (format nil "~C~C[2~CC" (code-char (+ 64 i)) #\Escape #\Backspace))))
      (feed s (format nil "~C~CA " #\Return #\Linefeed))
      (loop for i from 2 to 9 do
        (foundation-feed-octets s (octets (format nil "~C[~C~DC~C" #\Escape #\Return (- (* 2 i) 2) (code-char (+ 64 i))))))
      (feed s (format nil "~C~C" #\Return #\Linefeed))
      (feed s (csi "20" #\l))
      (loop for i from 1 to 9 do
        (foundation-feed-octets s (octets (format nil "~C ~C[1~CA" (code-char (+ 64 i)) #\Escape (code-char 11)))))
      (dotimes (y 4) (expect (string= "A B C D E F G H I" (row-string s y :end 17))))))

  (it "retains CSI parameters, separators, private marker and intermediate around controls"
    (with-screen (s 80 24)
      (foundation-feed-octets s #(27 91 49 0 50 59 17 51 19 52 127 72))
      (check-cursor s 33 11)
      (foundation-feed-octets s #(27 91 63 7 55 108))
      (expect (not (nerimux/terminal/types:screen-autowrap s)))
      (expect (nerimux/terminal/types:screen-bell-pending s))
      (foundation-feed-octets s #(27 91 50 32 0 113))
      (expect (= 2 (nerimux/terminal/types:screen-cursor-shape s)))))

  (it "executes SI and SO in CSI without losing the unfinished cursor command"
    (with-screen (s 20 3)
      (feed s (esc ")A"))
      (foundation-feed-octets s #(27 91 49 14 67 35 27 91 49 15 67 35))
      (expect (char= #\£ (char-at s 1 0)))
      (expect (char= #\# (char-at s 3 0)))))

  (it "cancels on CAN or SUB and restarts on ESC"
    (dolist (cancel '(24 26))
      (with-screen (s 20 4)
        (foundation-feed-octets s (vector 27 91 49 50 cancel 72))
        (check-cursor s 1 0)
        (expect (char= #\H (char-at s 0 0)))))
    (with-screen (s 20 4)
      (foundation-feed-octets s #(27 91 49 50 27 91 50 59 51 72 88))
      (expect (char= #\X (char-at s 2 1)))
      (check-cursor s 3 1)))

  (it-fuzz "embedded C0 has the same effect as executing it before CSI collection"
      ((row (gen-integer :min 2 :max 10))
       (column (gen-integer :min 2 :max 30))
       (distance (gen-integer :min 0 :max 60))
       (control (gen-member '(7 8 9 10 11 12 13))))
      (:trials 200 :timeout-per-trial 2)
    (with-screen (embedded 80 24)
      (with-screen (before 80 24)
        (dolist (s (list embedded before))
          (feed s (csi (format nil "~D;~D" row column) #\H)))
        (foundation-feed-octets embedded (octets (format nil "~C[~D~CC" #\Escape distance (code-char control))))
        (foundation-feed-octets before (vector control))
        (feed before (csi (write-to-string distance) #\C))
        (expect (= (screen-cursor-x before) (screen-cursor-x embedded)))
        (expect (= (screen-cursor-y before) (screen-cursor-y embedded)))
        (expect (eql (nerimux/terminal/types:screen-bell-pending before)
                     (nerimux/terminal/types:screen-bell-pending embedded)))))))

(describe "terminal-suite/vt100-foundation-charsets"
  (it "designates British G0/G1 and invokes them with SI/SO"
    (with-screen (s 40 3)
      (feed s (esc "(A"))
      (feed s "#@AZ")
      (feed s (esc ")B"))
      (foundation-feed-octets s #(14 35 15 35))
      (expect (string= "£@AZ#£" (row-string s 0 :end 6)))
      (feed s (esc "(B"))
      (feed s (esc ")A"))
      (foundation-feed-octets s #(35 14 35 15 35))
      (expect (string= "#£#" (row-string s 0 :start 6 :end 9)))))

  ;; User-selected software policy: optional ROMs are absent, so designators
  ;; 1/2 select standard ASCII. This does not assert a historical DEC ROM map.
  (it "falls back to standard ASCII for absent ROM 1/2 in either bank without invoking G1"
    (dolist (designator '("1" "2"))
      (dolist (bank '(:g0 :g1))
        (dolist (previous '(:british :dec-graphics))
          (with-screen (s 20 3)
            (let* ((other (if (eq previous :british) :dec-graphics :british))
                   (g0 (if (eq bank :g0) previous other))
                   (g1 (if (eq bank :g1) previous other)))
              (feed s (esc "(~A" (if (eq g0 :british) "A" "0")))
              (feed s (esc ")~A" (if (eq g1 :british) "A" "0")))
              (foundation-feed-octets s #(15 35 113 14 35 113 15))
              (expect (string= (if (eq g0 :british) "£q#─" "#─£q")
                               (row-string s 0 :end 4)))
              (feed s (esc "~A~A" (if (eq bank :g0) "(" ")") designator))
              (expect (eq (if (eq bank :g0) :ascii other)
                          (nerimux/terminal/types:screen-g0-charset s)))
              (expect (eq (if (eq bank :g1) :ascii other)
                          (nerimux/terminal/types:screen-g1-charset s)))
              (expect (eq :g0 (nerimux/terminal/types:screen-active-g s)))
              (expect (eq (if (eq bank :g0) :ascii other)
                          (nerimux/terminal/types:screen-charset s)))
              (feed s "#q")
              (foundation-feed-octets s #(14))
              (expect (eq :g1 (nerimux/terminal/types:screen-active-g s)))
              (expect (eq (if (eq bank :g1) :ascii other)
                          (nerimux/terminal/types:screen-charset s)))
              (feed s "#q")
              (foundation-feed-octets s #(15))
              (expect (eq :g0 (nerimux/terminal/types:screen-active-g s)))
              (feed s "#q")
              (expect (string= (if (eq bank :g0)
                                   (if (eq other :british) "#q£q#q" "#q#─#q")
                                   (if (eq other :british) "£q#q£q" "#─#q#─"))
                               (row-string s 0 :start 4 :end 10)))))))))

  (it "applies British single shifts once and restores ASCII on reset"
    (with-screen (s 20 3)
      (feed s (esc "*A"))
      (feed s (esc "N"))
      (feed s "##")
      (expect (string= "£#" (row-string s 0 :end 2)))
      (feed s (esc "(A"))
      (feed s (esc "c"))
      (feed s "#")
      (expect (char= #\# (char-at s 0 0))))))

(describe "terminal-suite/vt100-foundation-reports-and-input-contract"
  (it "emits one exact DECREPTPARM per default, zero or one request"
    (with-screen (s 80 24)
      (dolist (param '("" "0" "1")) (feed s (csi param #\x)))
      ;; Profile defined by VT100 UG DECREQTPARM: 112=9600, 1=no parity,
      ;; 1=8 bits, clkmul1=16, flags0. A virtual PTY has no physical baud rate.
      (expect (equal (list (esc "[2;1;1;112;112;1;0x")
                           (esc "[2;1;1;112;112;1;0x")
                           (esc "[3;1;1;112;112;1;0x"))
                     (reverse (nerimux/terminal/types:screen-response-queue s))))))

  (it "rejects unsupported or malformed DECREQTPARM without replying"
    (dolist (param '("2" "3" "99" "0;1" "1;0" "0:0" "1:0" "?0" ">0" "=0" "<0"
                     "0 " "0$" "0;" "1;" ";" ";;" "0;;" "1;;" "0;0" ";0"))
      (with-screen (s 80 24)
        (foundation-feed-octets s (map 'vector #'char-code (csi param #\x)))
        (expect (null (nerimux/terminal/types:screen-response-queue s)))
        (check-cursor s 0 0))))

  (it "accepts DECLL and DECTST silently without inventing hardware results"
    (with-screen (s 20 4)
      (feed s "host")
      (dolist (param '("" "0" "1;2;3;4")) (feed s (csi param #\q)))
      (dolist (param '("2;0" "2;1" "2;2" "2;4" "2;9")) (feed s (csi param #\y)))
      (expect (null (nerimux/terminal/types:screen-response-queue s)))
      (expect (string= "host" (row-string s 0 :end 4)))
      (check-cursor s 4 0)))

  (it "resets parser-enabled application cursor and keypad modes on RIS and DECSTR"
    (dolist (reset (list (esc "c") (csi "!" #\p)))
      (with-screen (s 20 4)
        (feed s (csi "?1" #\h))
        (feed s (esc "="))
        (expect (nerimux/terminal/types:screen-app-cursor-keys s))
        (expect (nerimux/terminal/types:screen-app-keypad s))
        (feed s reset)
        (expect (not (nerimux/terminal/types:screen-app-cursor-keys s)))
        (expect (not (nerimux/terminal/types:screen-app-keypad s)))
        (expect (not (nerimux/terminal/types:screen-vt52-mode s)))
        ;; A second reset must have the same power-on mode defaults.
        (feed s reset)
        (expect (not (nerimux/terminal/types:screen-app-cursor-keys s)))
        (expect (not (nerimux/terminal/types:screen-app-keypad s))))))

  (it "tracks DECKPAM/DECKPNM and resets both new input contract flags"
    (with-screen (s 20 4)
      (expect (not (nerimux/terminal/types:screen-app-keypad s)))
      (expect (not (nerimux/terminal/types:screen-vt52-mode s)))
      (feed s (esc "="))
      (expect (nerimux/terminal/types:screen-app-keypad s))
      (feed s (esc ">"))
      (expect (not (nerimux/terminal/types:screen-app-keypad s)))
      (dolist (reset (list (esc "c") (csi "!" #\p)))
        (feed s (esc "="))
        ;; Return to ANSI before its RIS/DECSTR commands; neither is VT52.
        (setf (nerimux/terminal/types:screen-vt52-mode s) t)
        (feed s (esc "<"))
        (feed s reset)
        (expect (not (nerimux/terminal/types:screen-app-keypad s)))
        (expect (not (nerimux/terminal/types:screen-vt52-mode s)))))))


(describe "terminal-suite/vt100-foundation-csi-defaults"
  (it "applies omitted and semicolon-default CUP coordinates"
    (dolist (case '(("" 0 0) (";" 0 0) ("3;" 0 2) (";4" 3 0) ("0;0" 0 0)))
      (with-screen (s 20 6)
        (feed s (csi "5;8" #\H))
        (feed s (csi (first case) #\H))
        (check-cursor s (second case) (third case)))))

  (it "defaults omitted and trailing-empty DECSTBM bounds"
    (dolist (case '(("" 0 5) (";" 0 5) ("2;" 1 5) (";4" 0 3)))
      (with-screen (s 20 6)
        (feed s (csi "3;5" #\r))
        (feed s (csi (first case) #\r))
        (expect (= (second case) (nerimux/terminal/types:screen-scroll-top s)))
        (expect (= (third case) (nerimux/terminal/types:screen-scroll-bottom s))))))

  (it "applies every SGR empty field as default zero in parameter order"
    (dolist (param '("" ";" "1;" "1;;"))
      (with-screen (s 20 4)
        (feed s (csi "1" #\m))
        (feed s (csi param #\m))
        (feed s "X")
        (expect (zerop (logand nerimux/terminal/types:+attr-bold+
                               (nerimux/terminal/types:cell-attrs
                                (nerimux/terminal/types:screen-cell s 0 0)))))))
    (with-screen (s 20 4)
      (feed s (csi ";1" #\m))
      (feed s "X")
      (expect (logtest nerimux/terminal/types:+attr-bold+
                       (nerimux/terminal/types:cell-attrs
                        (nerimux/terminal/types:screen-cell s 0 0)))))))
