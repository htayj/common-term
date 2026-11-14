(defpackage #:lispterm.gpu
  (:use #:cl)
  (:export #:gpu-unavailable #:gpu-operation #:gpu-detail #:unavailable
           #:open-context #:close-context #:context-info #:assert-context-owner
           #:emit-shader #:shader-form
           #:make-atlas #:atlas-begin-frame #:atlas-ensure-glyph #:atlas-epoch
           #:make-instance-buffer #:instance-buffer-write #:instance-buffer-octets
           #:make-renderer #:renderer-resize #:renderer-write-cell #:renderer-write-remainder
           #:renderer-draw #:renderer-close #:renderer-metrics #:renderer-readback))
