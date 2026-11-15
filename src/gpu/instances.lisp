(in-package #:lispterm.gpu)

(defconstant +instance-stride+ 32)
(defconstant +instance-bold+ 1)
(defconstant +instance-underline+ 2)
(defconstant +instance-hidden+ 4)

(defclass cell-instance-buffer ()
  ((columns :initarg :columns :reader instance-buffer-columns)
   (rows :initarg :rows :reader instance-buffer-rows)
   (slot-limit :initarg :slot-limit :reader instance-buffer-slot-limit)
   (octets :initarg :octets :reader instance-buffer-octets)
   (dirty :initarg :dirty :reader instance-buffer-dirty)))

(defun make-instance-buffer (&key columns rows (max-instances #xffffffff)
                                 (slot-limit #x100000000))
  "Allocate row-major 32-byte records. Limits are supplied from device queries.
SLOT-LIMIT is exclusive. OCTETS is borrowed read-only storage for GPU upload."
  (model-positive-integer columns :columns)
  (model-positive-integer rows :rows)
  (model-positive-integer max-instances :max-instances)
  (model-positive-integer slot-limit :slot-limit #x100000000)
  (let ((count (* columns rows)))
    (unless (and (<= count max-instances)
                 (< (* count +instance-stride+) array-total-size-limit))
      (error "Instance grid exceeds configured or Lisp capacity"))
    (make-instance 'cell-instance-buffer :columns columns :rows rows :slot-limit slot-limit
                   :octets (make-array (* count +instance-stride+)
                                       :element-type '(unsigned-byte 8) :initial-element 0)
                   :dirty (make-array count :element-type 'bit :initial-element 1))))

(defun instance-line-size-code (line-size)
  (or (position line-size '(:single :double-width :double-height-top :double-height-bottom))
      (error "Invalid instance line size: ~S" line-size)))

(defun instance-unsigned (value maximum name)
  (unless (and (integerp value) (<= 0 value maximum))
    (error "Invalid ~A: expected integer in [0,~D]" name maximum))
  value)

(defun instance-put-u32 (octets offset value)
  (dotimes (byte 4) (setf (aref octets (+ offset byte)) (ldb (byte 8 (* byte 8)) value))))

(defun instance-record-fields (columns rows slot-limit column row
                               &key (slot 0) (foreground 0) (background 0)
                                 bold underline hidden (span 1) (line-size :single))
  "Validate one complete record and return its eight uint32 fields in wire order:
column,row,slot,flags,fg,bg,span,line-code. SLOT-LIMIT is exclusive."
  (instance-unsigned column (1- columns) :column)
  (instance-unsigned row (1- rows) :row)
  (instance-unsigned slot (1- slot-limit) :slot)
  (instance-unsigned foreground #xffffff :foreground)
  (instance-unsigned background #xffffff :background)
  (unless (every (lambda (v) (member v '(nil t))) (list bold underline hidden))
    (error "Instance flags must be boolean"))
  (unless (member span '(1 2)) (error "Instance span must be 1 or 2"))
  (list column row slot
        (logior (if bold +instance-bold+ 0) (if underline +instance-underline+ 0)
                (if hidden +instance-hidden+ 0))
        foreground background span (instance-line-size-code line-size)))

(defun instance-store-record (octets offset fields)
  "Store FIELDS as little-endian uint32s at OFFSET when the bytes differ.
Return true when OCTETS changed."
  (let ((changed nil))
    (loop for value in fields for field from 0
          do (dotimes (byte 4)
               (unless (= (aref octets (+ offset (* field 4) byte))
                          (ldb (byte 8 (* byte 8)) value))
                 (setf changed t))))
    (when changed
      (loop for value in fields for field from 0
            do (instance-put-u32 octets (+ offset (* field 4)) value)))
    changed))

(defun instance-encode-record (octets offset columns rows slot-limit column row &rest fields)
  "The one definition of the 32-byte record: validate completely, then store at
OFFSET. FIELDS are the INSTANCE-BUFFER-WRITE keywords. Return true when OCTETS changed."
  (instance-store-record
   octets offset (apply #'instance-record-fields columns rows slot-limit column row fields)))

(defgeneric instance-buffer-write (buffer column row &key slot foreground background
                                                        bold underline hidden span line-size))
(defmethod instance-buffer-write ((buffer cell-instance-buffer) column row
                                 &key (slot 0) (foreground 0) (background 0)
                                   bold underline hidden (span 1) (line-size :single))
  "Validate the complete record before mutation; dirty only changed records.
Fields: column,row,slot,flags,fg,bg,span,line-code, all little-endian uint32."
  (let* ((fields (instance-record-fields
                  (instance-buffer-columns buffer) (instance-buffer-rows buffer)
                  (instance-buffer-slot-limit buffer) column row
                  :slot slot :foreground foreground :background background
                  :bold bold :underline underline :hidden hidden :span span :line-size line-size))
         (index (+ column (* row (instance-buffer-columns buffer)))))
    (when (instance-store-record (instance-buffer-octets buffer) (* index +instance-stride+) fields)
      (setf (sbit (instance-buffer-dirty buffer) index) 1)
      t)))

(defun instance-buffer-dirty-ranges (buffer)
  "Return merged half-open byte intervals. Retain dirty bits until upload succeeds."
  (let ((bits (instance-buffer-dirty buffer)) (start nil) (ranges nil))
    (loop for index from 0 below (length bits)
          do (if (= 1 (sbit bits index))
                 (unless start (setf start index))
                 (when start
                   (push (list (* start +instance-stride+) (* index +instance-stride+)) ranges)
                   (setf start nil))))
    (when start (push (list (* start +instance-stride+) (* (length bits) +instance-stride+)) ranges))
    (nreverse ranges)))

(defun instance-buffer-dirty-rows (buffer)
  (loop with bits = (instance-buffer-dirty buffer)
        with columns = (instance-buffer-columns buffer)
        for row below (instance-buffer-rows buffer)
        when (position 1 bits :start (* row columns) :end (* (1+ row) columns)) collect row))

(defun instance-buffer-clear-dirty (buffer)
  "Acknowledge successful upload of all reported ranges on the owner thread."
  (fill (instance-buffer-dirty buffer) 0)
  buffer)

(export '(+instance-stride+ +instance-bold+ +instance-underline+ +instance-hidden+
          instance-buffer-columns instance-buffer-rows instance-buffer-slot-limit
          instance-buffer-dirty-ranges instance-buffer-dirty-rows instance-buffer-clear-dirty
          instance-line-size-code))
