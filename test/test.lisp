(in-package :lisp-struct/test)

(defun lisp-struct-test ()
  ;; Test the unpacking code
  (let ((test-data #(#xF1 #xF2 #xF3 #xF4 #xF5 #xF6 #xF7 #xF8 #xF9 #xFA #xFB #xFC #xFD #xFE #xFF)))
    ;; Unpack the data
    (print (unpack "<hBBHbq" test-data)))

  ;; Test the packing code
  (print (pack ">bHL" (list -45 10000 34)))

  (run!)
  (fresh-line))

