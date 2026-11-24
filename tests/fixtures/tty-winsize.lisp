;;;; Read the Linux kernel winsize on the child's stdin in one ioctl.
;;;; Do not load Common Term or consult its display geometry.
(sb-alien:define-alien-routine ("ioctl" ioctl) sb-alien:int
  (fd sb-alien:int)
  (request sb-alien:unsigned-long)
  (size (* sb-alien:unsigned-short)))

(sb-alien:with-alien ((size (array sb-alien:unsigned-short 4)))
  (unless (zerop (ioctl 0 #x5413 (sb-alien:addr (sb-alien:deref size 0))))
    (error "TIOCGWINSZ on child stdin failed"))
  (format t "~D ~D ~D ~D~%"
          (sb-alien:deref size 0) (sb-alien:deref size 1)
          (sb-alien:deref size 2) (sb-alien:deref size 3)))