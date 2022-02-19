(defpackage :lisp-struct
  (:nicknames :struct)
  (:use :common-lisp :parseq)
  (:export pack unpack integer-limit-error use-value clip-value wrap-value))
