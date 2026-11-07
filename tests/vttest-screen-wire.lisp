;;;; Conditional source-byte fragments for vttest 2.7 (20251205).
;;;; Standalone: no product, recorder, terminal, or project dependencies.
(defpackage #:vttest-screen-wire
  (:use #:cl)
  (:export #:wire-refusal #:refusal-reason #:refusal-detail
           #:application-to-effective #:startup-fragment
           #:make-menu-parameters #:menu-cleanup-fragment
           #:fragment-id #:fragment-application-octets #:fragment-effective-octets
           #:fragment-application-length #:fragment-effective-length
           #:fragment-provenance #:fragment-preconditions))
(in-package #:vttest-screen-wire)

(define-condition wire-refusal (error)
  ((reason :initarg :reason :reader refusal-reason)
   (detail :initarg :detail :reader refusal-detail))
  (:report (lambda (condition stream)
             (format stream "Screen wire refusal (~S): ~A"
                     (refusal-reason condition) (refusal-detail condition)))))

(defun refuse (reason detail)
  (error 'wire-refusal :reason reason :detail detail))

(defun validate-octets (octets)
  ;; General vectors are accepted if every active element is an octet.
  ;; Strings, lists, multidimensional arrays and implicit encodings are refused.
  (unless (and (vectorp octets) (not (stringp octets)))
    (refuse :malformed-octets "Supply a non-string vector of integers 0..255."))
  (loop for byte across octets for index from 0
        unless (typep byte '(unsigned-byte 8))
          do (refuse :malformed-octets
                     (format nil "Element ~D is not an octet." index)))
  octets)

(defun application-to-effective (octets)
  "Apply conditional T ONCE to application octets, returning a fresh UB8 vector.
Each LF becomes CR LF; preserve existing CR and all other bytes. This is not
idempotent. Supply application bytes, never observed bytes or effective echo.
Named fragment effective accessors already return T's result."
  (validate-octets octets)
  (let ((result (make-array (+ (length octets) (count 10 octets))
                            :element-type '(unsigned-byte 8)))
        (position 0))
    (loop for byte across octets do
      (when (= byte 10)
        (setf (aref result position) 13)
        (incf position))
      (setf (aref result position) byte)
      (incf position))
    result))

(defun owned-copy (value)
  "Copy the complete acyclic metadata tree, including strings and vectors."
  (typecase value
    (cons (cons (owned-copy (car value)) (owned-copy (cdr value))))
    (string (copy-seq value))
    (vector (map (type-of value) #'owned-copy value))
    (t value)))

(defstruct (fragment (:constructor %make-fragment)
                     (:conc-name %fragment-) (:copier nil))
  (id nil :read-only t)
  (application #() :read-only t)
  (effective #() :read-only t)
  (provenance nil :read-only t)
  (preconditions nil :read-only t))

(defun fragment-id (fragment) (%fragment-id fragment))
(defun fragment-application-octets (fragment)
  "Return a fresh copy of the untransformed source application bytes."
  (copy-seq (%fragment-application fragment)))
(defun fragment-effective-octets (fragment)
  "Return a fresh copy of the once-transformed application bytes."
  (copy-seq (%fragment-effective fragment)))
(defun fragment-application-length (fragment)
  (length (%fragment-application fragment)))
(defun fragment-effective-length (fragment)
  (length (%fragment-effective fragment)))
(defun fragment-provenance (fragment)
  "Return independent source pins, helper dependencies and local source maps."
  (owned-copy (%fragment-provenance fragment)))
(defun fragment-preconditions (fragment)
  "Return conditional requirements, not receipts establishing their truth."
  (owned-copy (%fragment-preconditions fragment)))

(defun source-reference (member ranges)
  (list :member member :sha256
        (cond
          ((string= member "main.c")
           "4bd1d88d41c2cc346ae9ec858f11d87243eafbb829b4a0bf40168c45a364c3c1")
          ((string= member "setup.c")
           "f31ddc88180979c1c154b13bc6228ab5dd61021896088a18ddaf8e50987634cd")
          ((string= member "esc.c")
           "de36d3e70cf0d750ab9a444af9419b0fc45f08415430474e66b5e54a687a645d")
          ((string= member "charsets.c")
           "0b33ebeb34382620e526e470f8ecf90cc5e779bc604187ab957ca7a0fd0fa0e9")
          ((string= member "unix_io.c")
           "a92b41cc23ae55fdd35a36818a4ec1a69e9f0d2c6606d1dbb3f1e6f732f2ab73"))
        :inclusive-lines ranges))

(defun fragment-sources (id)
  (append
   (list (source-reference "main.c"
                           (case id
                             (:locale-prefix '((1418 1459) (1471 1478)))
                             (:primary-request '((1476 1478) (1576 1588)))
                             (:setup '((1478 1489)))
                             (:locale-suffix '((1461 1467) (1491 1494)))))
         (source-reference "esc.c"
                           ;; Common byte writers, 7-bit CSI and padding guard.
                           '((23 39) (144 151) (201 273) (406 426) (505 521))))
   (when (member id '(:primary-request :setup))
     (list (source-reference "setup.c" '((14 15) (48 194) (406 466)))))
   (when (eq id :primary-request)
     (list (source-reference "unix_io.c" '((152 176)))))))

(defun source-pieces (id locale)
  ;; Each piece is (application-octets main-line esc-helper inclusive-ranges).
  ;; No strings are encoded and no caller-supplied literal/provenance is used.
  (case id
    (:locale-prefix
     (when locale '(((27 37 64) 1453 :esc ((505 521))))))
    (:primary-request
     '(((27 91 48 99) 1478 :da ((635 638) (546 552)))))
    (:setup
     '(((27 91 63 49 108) 1479 :decckm ((712 719) (1271 1274)))
       ((27 91 63 51 108) 1480 :deccolm ((721 728) (1271 1274)))
       ((27 91 63 52 108) 1481 :decsclm ((953 961) (1271 1274)))
       ((27 91 63 53 108) 1482 :decscnm ((963 972) (1271 1274)))
       ((27 91 63 54 108) 1483 :decom ((857 864) (1271 1274)))
       ((27 91 63 55 104) 1484 :decawm ((672 679) (1336 1339)))
       ((27 91 63 56 108) 1485 :decarm ((657 664) (1271 1274)))
       ((27 91 63 52 48 104) 1486 :sm ((1336 1339)))
       ((27 91 63 52 53 108) 1487 :rm ((1271 1274)))
       ((27 91 114) 1488 :decstbm ((1036 1045) (505 521)))
       ((27 91 48 109) 1489 :sgr ((1320 1324)))))
    (:locale-suffix
     (when locale '(((27 37 71) 1465 :esc ((505 521))))))))

(defun startup-fragment (id &rest parameters)
  "Construct one closed conditional application fragment.
IDs are :LOCALE-PREFIX, :PRIMARY-REQUEST, :SETUP, :LOCALE-SUFFIX.
Supply :LOCALE T or NIL explicitly on every call, binding one choice for the
later composition. Only :VT100-AVO-LEVEL1 is supported as :PROFILE.
Construction neither accepts a reply nor establishes source/package, OS,
terminal or native custody. Duplicate/unknown parameters are refused."
  (unless (member id '(:locale-prefix :primary-request :setup :locale-suffix))
    (refuse :unsupported-fragment "Choose one of the four startup fragment IDs."))
  (unless (evenp (length parameters))
    (refuse :invalid-parameters "Supply keyword/value pairs."))
  (let ((seen nil))
    (loop for key in parameters by #'cddr do
      (unless (member key '(:locale :profile))
        (refuse :invalid-parameters "Only :LOCALE and :PROFILE are defined."))
      (when (member key seen)
        (refuse :invalid-parameters "Supply each parameter at most once."))
      (push key seen))
    (unless (member :locale seen)
      (refuse :missing-locale "Supply :LOCALE explicitly, including NIL.")))
  (let* ((locale (getf parameters :locale))
         (profile (getf parameters :profile :vt100-avo-level1))
         (offset 0)
         (source-map nil))
    (unless (eq profile :vt100-avo-level1)
      (refuse :unsupported-profile "Only the exact VT100-with-AVO level1 profile is defined."))
    (unless (or (eq locale t) (null locale))
      (refuse :invalid-locale "Locale must be exactly T or NIL."))
    (let* ((pieces (source-pieces id locale))
           (application (make-array (loop for piece in pieces sum (length (first piece)))
                                    :element-type '(unsigned-byte 8))))
      (dolist (piece pieces)
        (destructuring-bind (bytes main-line helper helper-ranges) piece
          (let ((end (+ offset (length bytes))))
            (replace application bytes :start1 offset)
            (push (list :application-range (list offset end)
                        :source (source-reference "main.c" (list (list main-line main-line)))
                        :helper helper
                        :helper-source (source-reference "esc.c" helper-ranges))
                  source-map)
            (setf offset end))))
      (let ((effective (application-to-effective application)))
        (%make-fragment
         :id id :application application :effective effective
         :provenance
         (owned-copy
          (list :archive-sha256 "cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc"
                :source-version '(2 7 20251205)
                :sources (fragment-sources id) :source-map (nreverse source-map)
                :wire-class :application :effective-transform :lf-to-crlf-once
                :application-length (length application)
                :effective-length (length effective)))
         :preconditions
         (owned-copy
          (list :conditional t :profile profile :locale locale
                :locale-placement '(:locale-prefix :primary-request :setup :locale-suffix)
                :source-package-binding :required-not-established
                :launch '(:fresh t :output-8bits nil :parse-7bits nil :padding nil
                          :allows-utf8 nil :successful-io-required t :replay nil)
                :locale-branch-witness :required-not-established
                :fragment-requires
                (case id
                  (:locale-prefix '(:before :discovery :fresh-levels (-1 -1)))
                  (:primary-request '(:after :locale-prefix :working-echo :off
                                      :reply-custody :not-established))
                  (:setup '(:after :primary-request
                            :accepted-reply (:octets #(27 91 63 49 59 50 99)
                                             :origin :reply :count 1 :echo #()
                                             :custody :required-not-established)
                            :cached-levels (:current 1 :maximum 1)
                            :level-adjust-output :empty))
                  (:locale-suffix '(:after :setup :same-locale-as :locale-prefix))))))))))


;;; Slice 2: closed source productions, with one parameter record per consumer.
(defstruct (menu-parameters (:constructor %make-menu-parameters) (:copier nil)
                            (:conc-name %menu-))
  (locale nil :read-only t)
  (speed 9600 :read-only t))

(defun make-menu-parameters (&rest parameters)
  "Bind the consumer's single locale and finite speed for BOTH menu occurrences.
This records conditional choices, not actual locale, baud or compiled macros."
  (unless (evenp (length parameters))
    (refuse :invalid-parameters "Supply keyword/value pairs."))
  (let ((seen nil))
    (loop for key in parameters by #'cddr do
      (unless (member key '(:locale :speed))
        (refuse :invalid-parameters "Only :LOCALE and :SPEED are defined."))
      (when (member key seen)
        (refuse :invalid-parameters "Supply each parameter at most once."))
      (push key seen))
    (unless (member :locale seen)
      (refuse :missing-locale "Supply :LOCALE explicitly, including NIL.")))
  (let ((locale (getf parameters :locale)) (speed (getf parameters :speed 9600)))
    (unless (or (eq locale t) (null locale))
      (refuse :invalid-locale "Locale must be exactly T or NIL."))
    (unless (and (integerp speed)
                 (member speed '(0 50 75 110 134 150 200 300 600 1200 1800
                                 2400 4800 9600 19200 38400 57600 115200)))
      (refuse :invalid-speed "Supply a source-bound integer speed, not decimal text."))
    (%make-menu-parameters :locale locale :speed speed)))

(defun menu-cleanup-pieces (id speed)
  ;; Fixed ASCII source literals; callers cannot supply text or escape sequences.
  ;; Each entry is (bytes inclusive-main-lines helper).
  (labels ((text (s) (map 'list #'char-code s))
           (control (s) (cons 27 (map 'list #'char-code s))))
    (case id
      (:menu
       (append
        (list (list (control "[2J") '(279 279) :vt-clear)
              (list (control "[3;10H") '(280 280) :title)
              (list (text "VT100 test program, version 2.7 (20251205)") '(131 134) :version)
              (list (control "[4;10H") '(282 282) :title))
        (unless (= speed 9600)
          (list (list (text (format nil "Line speed ~Dbd " speed)) '(287 288) :printxx)))
        (list (list (control "[5;10H") '(292 292) :title)
              (list (append (text "Choose test type:") '(13 10)) '(292 292) :println)
              (list (control "[6;1H") '(1672 1672) :vt-move)
              (list (control "[0J") '(1673 1673) :vt-clear)
              (list '(13 10) '(1675 1675) :println))
        (loop for description in
              '("Exit" "Test of cursor movements" "Test of screen features"
                "Test of character sets" "Test of double-sized characters"
                "Test of keyboard" "Test of terminal reports" "Test of VT52 mode"
                "Test of VT102 features (Insert/Delete Char/Line)" "Test of known bugs"
                "Test of reset and self-test" "Test non-VT100 (e.g., VT220, XTERM) terminals"
                "Modify test-parameters")
              for number from 0
              collect (list (append (text (format nil "          ~D. ~A" number description))
                                    '(10))
                            '(1634 1636) :show-entry))
        (list (list '(13 10) '(1684 1684) :println)
              (list (text "          Enter choice number (0 - 12): ") '(1685 1685) :printxx))))
      (:dispatch-clear (list (list (control "[2J") '(1740 1740) :vt-clear)))
      (:exit-cleanup
       (append
        (loop for s in '("[?1l" "[?3l" "[?5l" "[?6l" "[?7h" "[?8h" "[r" "[0m")
              for line in '(1504 1505 1506 1507 1508 1509 1510 1511)
              collect (list (control s) (list line line) :reset))
        (list (list (control "[2J") '(1515 1515) :vt-clear)
              (list (control "[12;30H") '(1516 1516) :vt-move)
              (list (append (text "That's all, folks!") '(10)) '(1517 1517) :printxx)
              (list '(10 10 10) '(1518 1518) :printxx)))))))

(defun menu-cleanup-fragment (id &rest arguments)
  "Emit :MENU, :DISPATCH-CLEAR or :EXIT-CLEANUP with :PARAMETERS bound once.
Reuse that same MAKE-MENU-PARAMETERS record for both menus and the locale
startup choices. This emitter does not enforce session composition or emit
input echo, holds or area bytes. Exit cleanup excludes the dispatch clear."
  (unless (member id '(:menu :dispatch-clear :exit-cleanup))
    (refuse :unsupported-fragment "Choose a menu/cleanup fragment ID."))
  (unless (and (= (length arguments) 2) (eq (first arguments) :parameters))
    (refuse :invalid-parameters "Supply exactly :PARAMETERS and its bound record."))
  (let ((parameters (second arguments)))
    (unless (typep parameters 'menu-parameters)
      (refuse :invalid-menu-parameters "Use MAKE-MENU-PARAMETERS."))
    (let* ((speed (%menu-speed parameters))
           (pieces (menu-cleanup-pieces id speed))
           (application (make-array (loop for p in pieces sum (length (first p)))
                                    :element-type '(unsigned-byte 8)))
           (offset 0) (source-map nil))
      (dolist (piece pieces)
        (destructuring-bind (bytes lines helper) piece
          (let ((end (+ offset (length bytes))))
            (replace application bytes :start1 offset)
            (push (list :application-range (list offset end)
                        :source (source-reference "main.c" (list lines)) :helper helper)
                  source-map)
            (setf offset end))))
      (let ((effective (application-to-effective application)))
        (%make-fragment
         :id id :application application :effective effective
         :provenance
         (owned-copy
          (list :archive-sha256 "cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc"
                :source-version '(2 7 20251205) :wire-class :application
                :effective-transform :lf-to-crlf-once
                :application-length (length application) :effective-length (length effective)
                :source-map (nreverse source-map)
                :sources (list (source-reference "main.c"
                               '((128 158) (278 295) (1496 1526) (1628 1685)
                                 (1739 1752) (1759 1763) (2052 2059) (2131 2154)))
                               (source-reference "esc.c"
                                '((144 197) (201 273) (300 426) (505 558)
                                  (619 625) (657 679) (712 728) (857 864)
                                  (963 972) (1036 1045) (1139 1143) (1271 1274)
                                  (1320 1339)))
                               (source-reference "setup.c" '((406 466))))))
         :preconditions
         (owned-copy
          (list :conditional t :profile :vt100-avo-level1
                :locale (%menu-locale parameters) :speed speed
                :global-parameter-reuse :same-record-for-both-menus-and-locale
                :locale-branch-witness :required-not-established
                :speed-branch-witness
                (if (member speed '(19200 38400 57600 115200))
                    :optional-compiled-speed-constant-required-not-established
                    :finite-source-speed-branch-required-not-established)
                :optional-speed-guards
                (case speed (19200 '(:or :B19200 :EXTA))
                            (38400 '(:or :B38400 :EXTB))
                            (57600 '(:defined :B57600))
                            (115200 '(:defined :B115200)))
                :speed-contract '(:document "screen-features-startup-source-20261107-a/SOURCE-RESOLUTION.md"
                                  :source "ttymodes.c" :table-lines (25 63)
                                  :initial-default-lines (187 192))
                :source-package-binding :required-not-established
                :launch '(:geometry (24 80 132) :current-menu "" :padding nil
                          :output-8bits nil :parse-7bits nil :allows-utf8 nil
                          :successful-io-required t :replay nil)
                :fragment-requires
                (case id
                  (:menu '(:after :locale-suffix-or-return-from-area
                           :input-read-custody :not-established))
                  (:dispatch-clear '(:after :accepted-choice-2-or-0
                                     :input-read-custody :not-established))
                  (:exit-cleanup '(:after :choice-0-dispatch-clear :cached-level 1
                                   :default-level-output :empty
                                   :inflush-close-success :required-not-established
                                   :final-application-lfs 4))))))))))

;;; Slice 3: source output before holds 1/2; hold text has one separate owner.
(export '(area-fragment hold-fragment))

(defun area-output-pieces (pause)
  ;; One piece per source command (including every loop iteration).
  (let ((pieces nil))
    (labels ((emit (line helper text)
               (push (list (map 'list #'char-code text) line helper) pieces))
             (ctl (line helper text)
               (emit line helper (concatenate 'string (string #\Escape) text)))
             (line-text (line text)
               (emit line :println (concatenate 'string text (string #\Return) (string #\Newline)))))
      (ecase pause
        (1
         (ctl 621 :cup "[1;1H") (ctl 622 :decawm "[?7h")
         (dotimes (i 160) (emit 624 :tprintf "*"))
         (ctl 625 :decawm "[?7l") (ctl 626 :cup "[3;1H")
         (dotimes (i 160) (emit 628 :tprintf "*"))
         (ctl 629 :decawm "[?7h") (ctl 630 :cup "[5;1H")
         (line-text 631 "This should be three identical lines of *'s completely filling")
         (line-text 632 "the top of the screen without any empty lines between.")
         (line-text 633 "(Test of WRAP AROUND mode setting.)"))
        (2
         (ctl 636 :ed "[2J") (ctl 637 :tbc "[3g") (ctl 638 :cup "[1;1H")
         (loop for col from 1 to 78 by 3 do
           (ctl 640 :cuf "[3C") (ctl 641 :hts "H"))
         (ctl 643 :cup "[1;4H")
         (loop for col from 4 to 78 by 6 do
           (ctl 645 :tbc "[0g") (ctl 646 :cuf "[6C"))
         (ctl 648 :cup "[1;7H") (ctl 649 :tbc "[1g") (ctl 650 :tbc "[2g")
         (ctl 651 :cup "[1;1H")
         (loop for col from 1 to 78 by 6 do
           (emit 653 :tprintf (concatenate 'string (string #\Tab) "*")))
         (ctl 654 :cup "[2;2H")
         (loop for col from 2 to 78 by 6 do (emit 656 :tprintf "     *"))
         (ctl 657 :cup "[4;1H")
         (line-text 658 "Test of TAB setting/resetting. These two lines")
         (emit 659 :printxx "should look the same. "))
        ((3 4 5 6)
         ;; Fixed predecessor sequence: light132, light80, dark132, dark80.
         ;; The reversal command belongs ONLY to the two 132-column phases.
         (let* ((wide (oddp pause)) (width (if wide 132 80))
                (background (if (< pause 5) "light" "dark")))
           (when wide
             (ctl (if (= pause 3) 665 663) :decscnm
                  (if (= pause 3) "[?5h" "[?5l")))
           (ctl (if wide 666 683) :deccolm (if wide "[?3h" "[?3l"))
           (ctl (if wide 667 684) :ed "[2J")
           (ctl (if wide 668 685) :cup "[1;1H")
           (when wide
             (ctl 669 :tbc "[3g")
             (loop for col from 1 to 132 by 8 do
               (ctl 671 :cuf "[8C") (ctl 672 :hts "H"))
             (ctl 674 :cup "[1;1H"))
           (loop for col from 1 to width by 10 do
             (emit (if wide 676 687) :tprintf
                   (subseq "1234567890" 0 (min 10 (- width col)))))
           (loop for row from 3 to 20 do
             (ctl (if wide 678 689) :cup (format nil "[~D;~DH" row row))
             (emit (if wide 679 690) :tprintf
                  (format nil "This is ~D column mode, ~A background." width background)))))
        ((7 8 9 10)
         ;; Both directions finish before this segment's separately owned hold.
         (let* ((soft (< pause 9)) (short (oddp pause))
                (first (if short 12 1)) (last (if short 13 24)))
           (when (= pause 7)
             (ctl 555 :ed "[2J") (ctl 556 :decom "[?6h"))
           (when short (ctl (if soft 559 561) :decsclm (if soft "[?4h" "[?4l")))
           (ctl 570 :decstbm (format nil "[~D;~Dr" first last))
           (ctl 571 :ed "[2J")
           (dolist (down '(nil t))
             (ctl (if down 574 576) (if down :cuu :cud) (if down "[24A" "[24B"))
             (loop for i from 1 to 29 do
               (emit 578 :printxx
                     (format nil "~A scroll ~A region [~D..~D] size ~D Line ~D~%"
                             (if soft "Soft" "Jump") (if down "down" "up")
                             first last (1+ (- last first)) i))
               (when down (ctl 583 :ri "M") (ctl 584 :ri "M"))))))
        (11
         (ctl 696 :ed "[2J") (ctl 697 :decstbm "[23;24r")
         (emit 698 :printxx
               (concatenate 'string (string #\Newline)
                            "Origin mode test. This line should be at the bottom of the screen."))
         (ctl 700 :cup "[1;1H")
         (emit 701 :tprintf "This line should be the one above the bottom of the screen. "))
        (12
         (ctl 704 :ed "[2J") (ctl 705 :decom "[?6l")
         (ctl 706 :cup "[24;1H")
         (emit 707 :tprintf "Origin mode test. This line should be at the bottom of the screen.")
         (ctl 709 :cup "[1;1H")
          (emit 710 :tprintf "This line should be at the top of the screen. "))
         (13
          (ctl 712 :decstbm "[1;24r") (ctl 714 :ed "[2J")
          (ctl 716 :cup "[1;20H") (emit 716 :tprintf "Graphic rendition test pattern:")
          (loop for row in '(4 4 6 6 8 8 10 10 12 12 14 14 16 16 18 18)
                for col in '(1 40 6 45 1 40 6 45 1 40 6 45 1 40 6 45)
                for attrs in '(("0") ("0;1") (";4") (";1" "4") ("0;5") ("0;5;1")
                               ("0;4;5") ("0;1;4;5") ("1;4;5;0;7") ("0;1;7")
                               ("0;4;7") ("0;1;4;7") ("1;4;;5;7") ("0;1;5;7")
                               ("0;4;5;7") ("0;1;4;5;7"))
                for text in '("vanilla" "bold" "underline" "bold underline" "blink" "bold blink"
                              "underline blink" "bold underline blink" "negative" "bold negative"
                              "underline negative" "bold underline negative" "blink negative"
                              "bold blink negative" "underline blink negative" "bold underline blink negative")
                for line from 717 do
                  (ctl line :cup (format nil "[~D;~DH" row col))
                  (dolist (attr attrs) (ctl line :sgr (concatenate 'string "[" attr "m")))
                  (emit line :tprintf text))
          (ctl 735 :sgr "[m") (ctl 737 :decscnm "[?5l")
          (ctl 738 :cup "[23;1H") (ctl 739 :el "[0K")
          (emit 740 :tprintf "Dark background. "))
         (14
          (ctl 743 :decscnm "[?5h") (ctl 744 :cup "[23;1H")
          (ctl 745 :el "[0K") (emit 746 :tprintf "Light background. "))
         (15
          (ctl 749 :decscnm "[?5l") (ctl 751 :ed "[2J")
          (loop for row in '(8 8 8 8 8 10 12 14 16)
                for col in '(12 24 36 48 60 1 1 1 1)
                for text in '("normal" "bold" "underscored" "blinking" "reversed"
                              "stars:" "line:" "x'es:" "diamonds:")
                for line from 753 do
                  (ctl line :cup (format nil "[~D;~DH" row col)) (emit line :tprintf text))
          (labels ((charset (line graphics)
                     ;; scs(0,c): designate G0, reset G1, select G0 with SI.
                     (emit line (if graphics :scs-graphics :scs-normal)
                           (coerce (list #\Escape #\( (if graphics #\0 #\B)
                                         #\Escape #\) #\B (code-char 15)) 'string))))
            (dotimes (cset 4)
              (dotimes (i 5)
                (ctl 766 :cup (format nil "[~D;~DH" (+ 10 (* 2 cset)) (+ 12 (* 12 i))))
                (ctl 767 :sgr (format nil "[;~Dm" (nth i '(0 1 4 5 7))))
                (charset (if (evenp cset) 769 771) (oddp cset))
                (dotimes (j 5) (emit 773 :tprintf (string (char "*qx`" cset))))
                (ctl 775 :decsc "7") (ctl 776 :cup (format nil "[~D;~DH" (1+ cset) (1+ i)))
                (ctl 777 :sgr "[m") (charset 778 nil) (emit 779 :tprintf "A") (ctl 780 :decrc "8")
                (dotimes (j 5) (emit 782 :tprintf (string (char "*qx`" cset))))))
            (ctl 787 :sgr "[0m") (charset 788 nil))
          (ctl 789 :cup "[21;1H")
          (line-text 790 "Test of the SAVE/RESTORE CURSOR feature. There should")
          (line-text 791 "be ten characters of each flavour, and a rectangle")
          (line-text 792 "of 5 x 4 A's filling the top left of the screen."))))
    (nreverse pieces)))

(defun output-fragment (id pieces preconditions)
  ;; All callers supply closed source productions, never user byte vectors.
  (let* ((application (make-array (loop for p in pieces sum (length (first p)))
                                  :element-type '(unsigned-byte 8)))
         (offset 0) (source-map nil))
    (dolist (piece pieces)
      (destructuring-bind (bytes line helper) piece
        (let ((end (+ offset (length bytes))))
          (replace application bytes :start1 offset)
          (push (list :application-range (list offset end) :helper helper
                      :source (source-reference (if (eq helper :holdit) "unix_io.c" "main.c")
                                                (list (list line line)))) source-map)
          (setf offset end))))
    (let ((effective (application-to-effective application)))
      (%make-fragment
       :id id :application application :effective effective
       :preconditions (owned-copy preconditions)
       :provenance
       (list :wire-class :application :effective-transform :lf-to-crlf-once
             :application-length (length application) :effective-length (length effective)
             :source-map (nreverse source-map)
               :sources (append (list (source-reference "main.c"
                                    (cond ((eq id :area-13) '((712 741)))
                                          ((eq id :area-14) '((743 747)))
                                          ((eq id :area-15) '((613 617) (749 795)))
                                          ((member id '(:area-11 :area-12)) '((696 711)))
                                         ((member id '(:area-7 :area-8 :area-9 :area-10))
                                         '((549 591) (695 695)))
                                        ((member id '(:area-3 :area-4 :area-5 :area-6))
                                         '((661 693)))
                                        (t '((619 660)))))
                             (source-reference "esc.c"
                                  (append '((144 197) (201 273) (275 358) (364 381)
                                            (505 558) (613 625) (672 679)
                                            (1170 1174) (1271 1274)
                                            (1336 1339) (1366 1370))
                                          (when (member id '(:area-3 :area-4 :area-5 :area-6))
                                            '((721 728) (963 972) (1139 1143)))
                                            (when (member id '(:area-13 :area-14 :area-15))
                                              '((23 39) (406 426) (899 902) (941 944)
                                                (963 972) (1036 1045) (1139 1143)
                                                (1299 1311) (1320 1324)))
                                            (when (member id '(:area-13 :area-14))
                                              '((1146 1150)))
                                            (when (eq id :area-15)
                                              '((477 487)))
                                           (when (member id '(:area-11 :area-12))
                                             '((23 39) (406 426) (857 864) (1036 1045) (1139 1143)))
                                          (when (member id '(:area-7 :area-8 :area-9 :area-10))
                                            '((23 39) (406 426) (606 610) (628 632)
                                              (857 864) (953 961) (1036 1045)
                                              (1139 1143) (1255 1259)))))
                              (source-reference "unix_io.c" '((237 242) (245 279))))
                              (when (eq id :area-15)
                                (list (source-reference "charsets.c" '((777 798)))))))))))

(defun area-fragment (pause &rest parameters)
  "Complete A1..A15 application output, excluding the separately owned hold.
Each finite identity has a fixed predecessor; no session or input is emitted."
  (unless (member pause '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
    (refuse :unsupported-area "Only screen output sequences 1 through 15 are defined."))
  (when parameters (refuse :invalid-parameters "Area output has no parameters."))
  (output-fragment
   (nth (1- pause) '(:area-1 :area-2 :area-3 :area-4 :area-5 :area-6
                      :area-7 :area-8 :area-9 :area-10 :area-11 :area-12 :area-13 :area-14 :area-15))
   (area-output-pieces pause)
   (list :conditional t :profile :vt100-avo-level1 :geometry '(24 80 132)
         :source-package-binding :required-not-established
         :launch '(:padding nil :output-8bits nil :parse-7bits nil :replay nil
                   :successful-io-required t)
         :before (nth (1- pause) '(:choice-2-dispatch-clear :hold-1-release
                                  :hold-2-release :hold-3-release :hold-4-release :hold-5-release
                                   :hold-6-release :hold-7-release :hold-8-release :hold-9-release
                                    :hold-10-release :hold-11-release :hold-12-release
                                    :hold-13-release :hold-14-release))
         :entry (case pause
                  (1 '(:normal-spec-contracts (1 2 3 4 5 6 7) :tty-crmod-success :required))
                  (2 '(:after-area-1-and-hold-1 :working-echo :off))
                   ((13 14 15)
                    (list :after-area-and-hold (1- pause) :working-echo :off
                          :columns 80 :global-reverse (= pause 15) :origin nil :autowrap t
                          :smooth-scroll nil :margins (if (= pause 13) '(23 24) '(1 24))
                          :cursor (case pause (13 '(1 60)) (14 '(23 31)) (15 '(23 32)))
                          :wrap-pending nil :attributes :normal :charset :ascii-g0-gl :lnm nil
                          :active-tabs '(9 17 25 33 41 49 57 65 73)
                          :tabs :logical-offscreen-retention-conditional
                          :normal-spec-contracts '(1 2 3 4 5 6 7 8 9)))
                  ((11 12)
                   (list :after-area-and-hold (1- pause) :working-echo :off
                         :columns 80 :global-reverse nil :origin t :autowrap t
                         :smooth-scroll nil :margins (if (= pause 11) '(1 24) '(23 24))
                         :cursor (if (= pause 11) '(1 14) '(23 74)) :wrap-pending nil
                         :active-tabs '(9 17 25 33 41 49 57 65 73)
                         :tabs :logical-offscreen-retention-conditional
                         :attributes :normal :charset :ascii-g0-gl :lnm nil
                         :normal-spec-contracts '(1 2 3 4 5 6 7 8 9)))
                  ((7 8 9 10)
                   (list :after-area-and-hold (1- pause) :working-echo :off
                         :columns 80 :global-reverse nil :autowrap t
                        :origin (/= pause 7) :smooth-scroll (not (null (member pause '(8 9))))
                         :margins (if (evenp pause) '(12 13) '(1 24))
                         :cursor (case pause (7 '(20 73)) (9 '(1 14)) (otherwise '(12 14)))
                         :wrap-pending nil :active-tabs '(9 17 25 33 41 49 57 65 73)
                         :tabs :logical-offscreen-retention-conditional
                         :normal-spec-contracts '(1 2 3 4 5 6 7 8)))
                  (otherwise
                   (list :after-area-and-hold (1- pause) :working-echo :off
                         :columns (if (member pause '(3 5)) 80 132)
                         :global-reverse (if (member pause '(4 5)) t nil)
                         :origin nil :margins '(1 24) :autowrap t
                         :normal-spec-contracts '(1 2 3 4 5 6 7))))
         :post (cond
                  ((member pause '(13 14 15))
                   (list :columns 80 :global-reverse (= pause 14) :origin nil :autowrap t
                         :smooth-scroll nil :margins '(1 24) :wrap-pending nil
                         :cursor (case pause (13 '(23 18)) (14 '(23 19)) (15 '(24 1)))
                         :attributes :normal :charset :ascii-g0-gl :lnm nil
                         :active-tabs '(9 17 25 33 41 49 57 65 73)
                         :tabs :logical-offscreen-retention-conditional
                         :saved-cursor (when (= pause 15)
                                         '(:cursor (16 65) :attributes (:reverse)
                                           :g0 :dec-special :g1 :ascii :gl 0 :origin nil
                                           :wrap-pending nil))
                         :working-echo (if (= pause 15) :restored-at-entry :off)))
                 ((member pause '(11 12))
                  (list :columns 80 :global-reverse nil :origin (= pause 11) :autowrap t
                        :smooth-scroll nil :margins '(23 24)
                        :cursor (if (= pause 11) '(23 61) '(1 47)) :wrap-pending nil
                        :active-tabs '(9 17 25 33 41 49 57 65 73)
                        :tabs :logical-offscreen-retention-conditional
                        :attributes :normal :charset :ascii-g0-gl :lnm nil))
                 ((<= 7 pause 10)
                   (list :columns 80 :global-reverse nil :origin t :autowrap t
                         :smooth-scroll (< pause 9) :margins (if (oddp pause) '(12 13) '(1 24))
                         :cursor (if (oddp pause) '(12 1) '(1 1)) :wrap-pending nil
                         :active-tabs '(9 17 25 33 41 49 57 65 73)
                         :tabs :logical-offscreen-retention-conditional
                          :timing-and-smooth-cadence :not-established))
                 ((<= 3 pause 6)
                    (list :columns (if (oddp pause) 132 80)
                          :global-reverse (< pause 5) :cursor
                          (list 20 (nth (- pause 3) '(62 61 61 60)))
                          :wrap-pending nil :autowrap t :origin nil :margins '(1 24)
                          :tabs :logical-offscreen-retention-conditional
                          :active-tabs (if (oddp pause)
                                           '(9 17 25 33 41 49 57 65 73 81 89 97 105 113 121 129 132)
                                           '(9 17 25 33 41 49 57 65 73)))))
         :hold-owner :hold-fragment :hold-identity pause :contains-hold nil)))

(defun hold-fragment (identity &rest parameters)
  "Shared holdit's output only. Identity 15 requires restored modes before output.
The inflush/readnl actions, input custody and any effective echo are not output here."
  (unless (and (integerp identity) (<= 1 identity 15))
    (refuse :unsupported-hold "Supply one of the fifteen screen hold identities."))
  (when parameters (refuse :invalid-parameters "Hold output has no parameters."))
  (output-fragment
   :hold (list (list (map 'list #'char-code "Push <RETURN>") 240 :holdit))
   (list :conditional t :hold-identity identity :after (list :area identity)
         :inflush-success :required-not-established :replay nil
         :before-input-read t :input-and-echo-custody :not-established
         :terminal-mode (if (= identity 15) :restored-before-hold :working-echo-off)
         :effective-echo-emitted nil)))
