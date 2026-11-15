(in-package #:lispterm.gpu)

(define-condition atlas-capacity-error (error)
  ((required :initarg :required :reader atlas-required-capacity)
   (maximum :initarg :maximum :reader atlas-maximum-capacity))
  (:report (lambda (c s) (format s "Frame needs ~D atlas tiles; maximum is ~D"
                                (atlas-required-capacity c) (atlas-maximum-capacity c)))))

(defun model-positive-integer (value name &optional (maximum #xffffffff))
  (unless (and (integerp value) (<= 1 value maximum))
    (error "~A must be an integer in [1, ~D], got ~S" name maximum value))
  value)

(defun snapshot-glyph-key (key)
  "Validate and detach an EQUAL key from caller-owned mutable data."
  (unless (and (listp key) (eql (list-length key) 3))
    (error "Glyph key must be a proper three-element (grapheme boolean-bold span) list"))
  (destructuring-bind (grapheme bold span) key
    (unless (or (characterp grapheme) (and (stringp grapheme) (plusp (length grapheme))))
      (error "Glyph grapheme must be a character or nonempty string"))
    (unless (member bold '(nil t)) (error "Glyph bold must be boolean"))
    (unless (member span '(1 2)) (error "Glyph span must be 1 or 2"))
    (list (if (stringp grapheme) (copy-seq grapheme) grapheme) bold span)))

(defstruct atlas-entry key mask (pending-p t))
(defclass glyph-atlas ()
  ((tile-width :initarg :tile-width :reader atlas-tile-width)
   (tile-height :initarg :tile-height :reader atlas-tile-height)
   (maximum :initarg :maximum :reader atlas-max-capacity)
   (entries :initarg :entries :accessor atlas-entries)
   (index :initform (make-hash-table :test 'equal) :reader atlas-index)
   (protected :initform (make-hash-table :test 'equal) :accessor atlas-protected)
   ;; Each cursor advances monotonically during a frame. Slots only become
   ;; occupied/protected, so neither search needs to revisit an earlier slot.
   (free-cursor :initform 0 :accessor atlas-free-cursor)
   (eviction-cursor :initform 0 :accessor atlas-eviction-cursor)
   (candidate-probes :initform 0 :accessor atlas-candidate-probes)
   (epoch :initform 0 :accessor atlas-epoch)))

(defun make-atlas (&key tile-width tile-height capacity max-capacity)
  "Construct bounded CPU glyph storage. Supply device-derived tile limits."
  (mapc (lambda (pair) (model-positive-integer (car pair) (cdr pair)))
        (list (cons tile-width :tile-width) (cons tile-height :tile-height)
              (cons capacity :capacity) (cons max-capacity :max-capacity)))
  (unless (<= capacity max-capacity) (error "Initial capacity exceeds maximum"))
  (when (>= (* tile-width tile-height max-capacity) array-total-size-limit)
    (error "Atlas exceeds Lisp array limit"))
  (make-instance 'glyph-atlas :tile-width tile-width :tile-height tile-height
                 :maximum max-capacity :entries (make-array capacity :initial-element nil)))

(defun atlas-capacity (atlas) (length (atlas-entries atlas)))

(defun grow-atlas (atlas required)
  (when (> required (atlas-capacity atlas))
    (when (> required (atlas-max-capacity atlas))
      (error 'atlas-capacity-error :required required :maximum (atlas-max-capacity atlas)))
    (let* ((old (atlas-entries atlas))
           (new (make-array (min (atlas-max-capacity atlas)
                                (max required (* 2 (length old)))) :initial-element nil)))
      (replace new old)
      (setf (atlas-entries atlas) new)
      (incf (atlas-epoch atlas))
      ;; Reallocating GPU texture storage loses all resident texels.
      (loop for entry across new when entry do (setf (atlas-entry-pending-p entry) t)))))

(defgeneric atlas-begin-frame (atlas keys))
(defmethod atlas-begin-frame ((atlas glyph-atlas) keys)
  "Protect every current-frame key atomically before allowing any eviction."
  (unless (and (listp keys) (list-length keys)) (error "Frame keys must be a proper finite list"))
  (let ((protected (make-hash-table :test 'equal)))
    (dolist (key keys)
      (let ((snapshot (snapshot-glyph-key key)))
        (unless (gethash snapshot protected)
          (when (= (hash-table-count protected) (atlas-max-capacity atlas))
            (error 'atlas-capacity-error :required (1+ (hash-table-count protected))
                   :maximum (atlas-max-capacity atlas)))
          (setf (gethash snapshot protected) t))))
    (grow-atlas atlas (hash-table-count protected))
    (setf (atlas-protected atlas) protected
          (atlas-free-cursor atlas) 0
          (atlas-eviction-cursor atlas) 0
          (atlas-candidate-probes atlas) 0))
  (atlas-epoch atlas))

(defun checked-mask (atlas mask)
  (unless (and (typep mask '(array (unsigned-byte 8) (*)))
               (= (length mask) (* (atlas-tile-width atlas) (atlas-tile-height atlas)))
               (every (lambda (v) (or (= v 0) (= v 255))) mask))
    (error "Mask must be a binary row-major UB8 tile"))
  mask)

(defun atlas-next-candidate (atlas free-p)
  "Consume the next ascending free or unprotected slot, inspecting each once.
ATLAS-CANDIDATE-PROBES counts actual slot reads for per-frame profiling."
  (let* ((entries (atlas-entries atlas))
         (capacity (length entries))
         (slot (loop for i from (if free-p (atlas-free-cursor atlas)
                                          (atlas-eviction-cursor atlas))
                     below capacity
                     for entry = (progn (incf (atlas-candidate-probes atlas))
                                        (aref entries i))
                     when (if free-p (null entry)
                              (and entry (not (gethash (atlas-entry-key entry)
                                                      (atlas-protected atlas)))))
                       return i)))
    (if free-p
        (setf (atlas-free-cursor atlas) (if slot (1+ slot) capacity))
        (setf (atlas-eviction-cursor atlas) (if slot (1+ slot) capacity)))
    slot))

(defgeneric atlas-ensure-glyph (atlas key mask))
(defmethod atlas-ensure-glyph ((atlas glyph-atlas) key mask)
  (let ((key (snapshot-glyph-key key)))
    (checked-mask atlas mask)
    (unless (gethash key (atlas-protected atlas))
      (error "Glyph was not declared by atlas-begin-frame"))
    (multiple-value-bind (slot found) (gethash key (atlas-index atlas))
      (when found
        (let ((entry (aref (atlas-entries atlas) slot)))
          (unless (equalp mask (atlas-entry-mask entry))
            (error "A glyph key cannot change its mask; use a new key or atlas"))
          (return-from atlas-ensure-glyph
            (values slot (atlas-epoch atlas) (atlas-entry-pending-p entry))))))
    (let* ((entries (atlas-entries atlas))
           (slot (or (atlas-next-candidate atlas t)
                     (atlas-next-candidate atlas nil))))
      (unless slot
        (grow-atlas atlas (1+ (length entries)))
        (setf entries (atlas-entries atlas)
              slot (atlas-next-candidate atlas t)))
      (let ((old (aref entries slot)))
        (when old
          (remhash (atlas-entry-key old) (atlas-index atlas))
          (incf (atlas-epoch atlas))))
      (setf (aref entries slot) (make-atlas-entry :key key :mask (copy-seq mask))
            (gethash key (atlas-index atlas)) slot)
      (values slot (atlas-epoch atlas) t))))

(defun atlas-resolve-settled-glyph (atlas key mask)
  "Resolve KEY between full frames, without eviction or growth.
Validate key and mask as ATLAS-ENSURE-GLYPH does, including the changed-mask
error. Return slot, epoch and pending flag for a resident glyph, or after filling
the next free slot. Return NIL with the entries untouched when the glyph needs
eviction or growth: the caller then runs a full ATLAS-BEGIN-FRAME frame.
The epoch never moves here. Free slots never reappear, so the free cursor of the
last full frame stays a valid lower bound."
  (let ((key (snapshot-glyph-key key)))
    (checked-mask atlas mask)
    (multiple-value-bind (slot found) (gethash key (atlas-index atlas))
      (if found
          (let ((entry (aref (atlas-entries atlas) slot)))
            (unless (equalp mask (atlas-entry-mask entry))
              (error "A glyph key cannot change its mask; use a new key or atlas"))
            (setf (gethash key (atlas-protected atlas)) t)
            (values slot (atlas-epoch atlas) (atlas-entry-pending-p entry)))
          (let ((slot (atlas-next-candidate atlas t)))
            (when slot
              (setf (aref (atlas-entries atlas) slot) (make-atlas-entry :key key :mask (copy-seq mask))
                    (gethash key (atlas-index atlas)) slot
                    (gethash key (atlas-protected atlas)) t)
              (values slot (atlas-epoch atlas) t)))))))

(defun atlas-reference-valid-p (atlas slot epoch &optional key)
  "Epoch references are conservative: any eviction invalidates every old reference."
  (and (eql epoch (atlas-epoch atlas)) (integerp slot)
       (<= 0 slot) (< slot (atlas-capacity atlas))
       (let ((entry (aref (atlas-entries atlas) slot)))
         (and entry (or (null key) (equal (snapshot-glyph-key key) (atlas-entry-key entry))) t))))

(defun atlas-glyph-snapshots (atlas pending-only)
  (loop for entry across (atlas-entries atlas) for slot from 0
        when (and entry (or (not pending-only) (atlas-entry-pending-p entry)))
          collect (list :slot slot :epoch (atlas-epoch atlas)
                        :key (snapshot-glyph-key (atlas-entry-key entry))
                        :mask (copy-seq (atlas-entry-mask entry)))))
(defun atlas-live-glyphs (atlas) (atlas-glyph-snapshots atlas nil))
(defun atlas-pending-uploads (atlas) (atlas-glyph-snapshots atlas t))
(defun atlas-acknowledge-upload (atlas slot epoch)
  "Acknowledge only after successful upload. Stale acknowledgements signal."
  (unless (atlas-reference-valid-p atlas slot epoch) (error "Stale atlas upload acknowledgement"))
  (setf (atlas-entry-pending-p (aref (atlas-entries atlas) slot)) nil)
  t)

(export '(atlas-capacity-error atlas-required-capacity atlas-maximum-capacity
          atlas-capacity atlas-max-capacity atlas-tile-width atlas-tile-height
          atlas-reference-valid-p atlas-live-glyphs atlas-pending-uploads atlas-acknowledge-upload))
