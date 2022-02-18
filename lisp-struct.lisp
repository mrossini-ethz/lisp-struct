;; lisp-struct
;; ----------------------------------------------------------------------------------------
;; This code implements something similar to the 'struct' module from python. The 'struct'
;; module allows the user to extract integers (and other values) from an array of bytes.
;; For example, a long integer is encoded as four bytes in the array in either the little-
;; endian (least significant byte first) or big-endian (most significant byte first). The
;; integer can be either signed or unsigned. In python the bytearray could be stored in a
;; variable, e.g. 'data'. The function
;;
;;     tuple_of_integers = struct.unpack("<LhbBH", data)
;;
;; will extract five integers from the data in the following order: an unsigned long integer
;; (L), a signed short integer (h), a signed charater (b), an unsigned character (B) and an
;; unsigned short integer. The first character in the string (<) means that all integers are
;; stored in little-endian byte order. The opposite would be (>), i.e. big endian.
;;
;; In lisp, we would like to do something similar. We would like to write the following:
;;
;;     (setf list-of-integers (struct:unpack "<LhbBH" data))
;;
;; However, we might want struct-unpack to be a macro such that the string "<LhbBH" will be
;; interpreted at compile time and the code will run faster.

;; Load the parseq package
(require :asdf)
(asdf:load-system :parseq)
(use-package :parseq)

;; --- Helper functions/macros -----------------------------------------------------

;; Helper macro: like (incf ...) but returning the old instead of the new value
(defmacro post-incf (place &optional (delta 1))
  (let ((var (gensym)))
    `(let ((,var ,place))
       (incf ,place ,delta)
       ,var)))

;; Function that converts an unsigned integer uint into a signed integer by interpreting
;; the unsigned value as the two's complement of the signed value.
(defun unsigned-to-signed (uint length)
  ;; Check data types
  (check-type length (integer 1))
  (check-type uint (integer 0))
  ;; Check integer size
  (let ((maxval (ash 1 (* 8 length))))
    (when (>= uint maxval)
      (error "Integer conversion error: value ~a exceeds integer size (~a)!" uint length))
    (if (< uint (/ maxval 2))
        uint
        (- uint maxval))))

;; Produces code to unpack an unsigned integer of given length and byte order from an array of bytes at the given position.
;; This function will be used in a macro expansion.
(defun unpack-unsigned (array pos length byte-order)
  `(+ ,@(loop for i below length
              for index = (* i 8)
              for shift = (case byte-order (:little-endian (+ pos i)) (:big-endian (- (+ pos length) i 1)) (t (error "Invalid byte order specified")))
              collect `(ash (elt ,array ,shift) ,index))))

;; Produces code to unpack a signed integer of given length and byte order from an array of bytes at the given position.
;; This function will be used in a macro expansion.
(defun unpack-signed (array pos length byte-order)
  `(unsigned-to-signed ,(unpack-unsigned array pos length byte-order) ,length))

;; --- Parseq rules ----------------------------------------------------------------

;; Parseq rule for generating the code that processes the data
(defrule format (array-var) (and (? alignment) (* (format-char array-var)))
  (:let (align :little-endian) (index 0))
  (:lambda (a f)
    (declare (ignore a))
    `(list ,@f)))

;; Parseq rule for processing the individual data type elements of the format string
(defrule format-char (array-var) (or (unsigned-char array-var)
                                     (signed-char array-var)
                                     (unsigned-short array-var)
                                     (signed-short array-var)
                                     (unsigned-long array-var)
                                     (signed-long array-var)
                                     (unsigned-long-long array-var)
                                     (signed-long-long array-var)))

;; Parseq rule for processing the byte order in the format string
(defrule alignment () (or #\< #\>)
  (:external align)
  (:lambda (x)
    (if (char= x #\<) (setf align :little-endian) (setf align :big-endian)) x))

;; Macro that helps defining rules for the different integer types
(defmacro define-integer-rule (character length signedness variable)
  `(defrule ,variable (array-var) ,character
     (:external align index)
     (:lambda (x)
       (declare (ignore x))
       ,(case signedness
         (:unsigned `(unpack-unsigned array-var (post-incf index ,length) ,length align))
         (:signed `(unpack-signed array-var (post-incf index ,length) ,length align))
         (t (error "Invalid signedness specified!"))))))

;; Use the helper macro to define the integer type rules
(define-integer-rule #\Q 8 :unsigned unsigned-long-long)
(define-integer-rule #\q 8 :signed signed-long-long)
(define-integer-rule #\L 4 :unsigned unsigned-long)
(define-integer-rule #\l 4 :signed signed-long)
(define-integer-rule #\H 2 :unsigned unsigned-short)
(define-integer-rule #\h 2 :signed signed-short)
(define-integer-rule #\B 1 :unsigned unsigned-char)
(define-integer-rule #\b 1 :signed signed-char)

;; --- Main macro definitions ------------------------------------------------------

;; Define the unpack macro
(defmacro unpack (format data)
  ;; Check the format string
  (unless (stringp format)
    (error "The argument 'format' must be a literal string!"))
  ;; Evaluate the data once
  (let ((array-var (gensym)))
    `(let ((,array-var ,data))
       ;; Generate the code for unpacking using the parseq rule
       ,(parseq `(format ,array-var) format))))

;; --- Test area -------------------------------------------------------------------

;; Test the code
(let ((test-data #(#xF1 #xF2 #xF3 #xF4 #xF5 #xF6 #xF7 #xF8 #xF9 #xFA #xFB #xFC #xFD #xFE #xFF)))
  ;; Unpack the data
  (unpack "<hBBHbq" test-data))
