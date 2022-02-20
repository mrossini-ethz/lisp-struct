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

;; --- Helper functions/macros -----------------------------------------------------

(in-package :lisp-struct)

(defmacro f-error (type (&rest initargs) control &rest args)
  "Like (error ...), but allows the condition type to be specified (which is required to inherit from simple-condition)."
  `(error ',type ,@initargs :format-control ,control :format-arguments (list ,@args)))

;; Helper macro: like (incf ...) but returning the old instead of the new value
(defmacro post-incf (place &optional (delta 1))
  (let ((var (gensym)))
    `(let ((,var ,place))
       (incf ,place ,delta)
       ,var)))

;; Function that converts an unsigned integer uint into a signed integer by interpreting
;; the unsigned value as the two's complement of the signed value.
(defun unsigned-to-signed (uint length)
  ;; Check integer size
  (let ((maxval (ash 1 (* 8 length))))
    (if (< uint (ash maxval -1))
        uint
        (- uint maxval))))

;; Function that converts a signed integer sint into an unsigned integer by encoding the
;; signed value usig two's complement.
(defun signed-to-unsigned (sint length)
  (if (minusp sint)
      (- (ash 1 (* 8 length)) (abs sint))
      sint))

(define-condition integer-limit-error (error) ())

;; Checks whether the given integer fits into the specified number of bytes.
(defun integer-limit-unsigned (integer bytes)
  (let ((maxval (1- (ash 1 (* 8 bytes)))))
    (if (and (>= integer 0) (<= integer maxval))
        integer
        (restart-case (error 'integer-limit-error)
          (use-value (value) value)
          (clip-value () (if (minusp integer) 0 maxval))
          (wrap-value () (mod integer (1+ maxval)))))))

;; Checks whether the given integer fits into the specified number of bytes when
;; using two's complement.
(defun integer-limit-signed (integer bytes)
  (let ((maxval (ash 1 (1- (* 8 bytes)))))
    (if (and (>= integer (- maxval)) (< integer maxval))
        integer
        (restart-case (error 'integer-limit-error)
          (use-value (value) (signed-to-unsigned value bytes))
          (clip-value () (if (minusp integer) (- maxval) (1- maxval)))))))

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

;; Produces code to pack an unsigned integer of given length into an array of bytes in given byte order and at the given position.
;; This function will be used in a macro expansion.
(defun pack-unsigned (pos length byte-order)
  (declare (ignore pos))
  (let ((value (gensym)))
    (loop for i below length
          for index = (case byte-order (:little-endian (* i 8)) (:big-endian (* (- length i 1) 8)) (t (error "Invalid byte order specified")))
          collect `(ldb (byte 8 ,index) ,value)
            into results
          finally (return `(,value (integer-limit-unsigned ,value ,length) ,results)))))

;; Produces code to pack a signed integer of given length into an array of bytes in given byte order and at the given position.
;; This function will be used in a macro expansion.
(defun pack-signed (pos length byte-order)
  (declare (ignore pos))
  (let ((value (gensym)))
    (loop for i below length
          for index = (case byte-order (:little-endian (* i 8)) (:big-endian (* (- length i 1) 8)) (t (error "Invalid byte order specified")))
          collect `(ldb (byte 8 ,index) ,value)
            into results
          finally (return `(,value (signed-to-unsigned (integer-limit-signed ,value ,length) ,length) ,results)))))

;; --- Parseq rules ----------------------------------------------------------------

;; Parseq rule for generating the code that processes the data for unpacking
(defrule unpack-format (array-var) (and alignment (* (unpack-format-char array-var)))
  (:let (align :little-endian) (index 0))
  (:lambda (a code) (declare (ignore a)) `(,index ,(apply #'concatenate 'list code))))

;; Parseq rule for generating the code that processes the data for packing
(defrule pack-format () (and alignment (* (pack-format-char)))
  (:let (align :little-endian) (index 0))
  (:lambda (a code) (declare (ignore a)) `(,index ,(apply #'concatenate 'list code))))

;; Parseq rule for processing the byte order in the format string
(defrule alignment () (or #\< #\>)
  (:external align)
  (:lambda (x)
    (if (char= x #\<) (setf align :little-endian) (setf align :big-endian)) x))

;; Parseq rule for numbers (used for character repetition)
(defrule rep-number () (and (char "1-9") (* (char "0-9")))
  (:string)
  (:function #'parse-integer))

;; Parseq rule for character repetition (defaults to 1 repetition)
(defrule reps () (? rep-number)
  (:lambda (&rest n) (if n (first n) 1)))

;; Parseq rule for unpacking the individual data type elements of the format string
(defrule unpack-format-char (array-var) (or (unpack-unsigned-char array-var)
                                            (unpack-signed-char array-var)
                                            (unpack-unsigned-short array-var)
                                            (unpack-signed-short array-var)
                                            (unpack-unsigned-long array-var)
                                            (unpack-signed-long array-var)
                                            (unpack-unsigned-long-long array-var)
                                            (unpack-signed-long-long array-var)))

;; Parseq rule for packing the individual data type elements of the format string
(defrule pack-format-char () (or (pack-unsigned-char)
                                 (pack-signed-char)
                                 (pack-unsigned-short)
                                 (pack-signed-short)
                                 (pack-unsigned-long)
                                 (pack-signed-long)
                                 (pack-unsigned-long-long)
                                 (pack-signed-long-long)))

;; Macro that helps defining unpack rules for the different integer types
(defmacro define-integer-unpack-rule (character length signedness variable)
  `(defrule ,variable (array-var) (and reps ,character)
     (:external align index)
     (:lambda (n c)
       (declare (ignore c))
       ,(case signedness
         (:unsigned `(loop for i below n collect (unpack-unsigned array-var (post-incf index ,length) ,length align)))
         (:signed `(loop for i below n collect (unpack-signed array-var (post-incf index ,length) ,length align)))
         (t (error "Invalid signedness specified!"))))))

;; Macro that helps defining pack rules for the different integer types
(defmacro define-integer-pack-rule (character length signedness variable)
  `(defrule ,variable () (and reps ,character)
     (:external align index)
     (:lambda (n c)
       (declare (ignore c))
       ,(case signedness
         (:unsigned `(loop for i below n collect (pack-unsigned (post-incf index ,length) ,length align)))
         (:signed `(loop for i below n collect (pack-signed (post-incf index ,length) ,length align)))
         (t (error "Invalid signedness specified!"))))))

;; Use the helper macro to define the integer type unpack rules
(define-integer-unpack-rule #\Q 8 :unsigned unpack-unsigned-long-long)
(define-integer-unpack-rule #\q 8 :signed unpack-signed-long-long)
(define-integer-unpack-rule #\L 4 :unsigned unpack-unsigned-long)
(define-integer-unpack-rule #\l 4 :signed unpack-signed-long)
(define-integer-unpack-rule #\H 2 :unsigned unpack-unsigned-short)
(define-integer-unpack-rule #\h 2 :signed unpack-signed-short)
(define-integer-unpack-rule #\B 1 :unsigned unpack-unsigned-char)
(define-integer-unpack-rule #\b 1 :signed unpack-signed-char)

;; Use the helper macro to define the integer type pack rules
(define-integer-pack-rule #\Q 8 :unsigned pack-unsigned-long-long)
(define-integer-pack-rule #\q 8 :signed pack-signed-long-long)
(define-integer-pack-rule #\L 4 :unsigned pack-unsigned-long)
(define-integer-pack-rule #\l 4 :signed pack-signed-long)
(define-integer-pack-rule #\H 2 :unsigned pack-unsigned-short)
(define-integer-pack-rule #\h 2 :signed pack-signed-short)
(define-integer-pack-rule #\B 1 :unsigned pack-unsigned-char)
(define-integer-pack-rule #\b 1 :signed pack-signed-char)

;; --- Main macro definitions ------------------------------------------------------

(define-condition argument-error (simple-condition) ())

;; Define the unpack macro
(defmacro unpack (format data)
  ;; Check the format string
  (unless (stringp format)
    (f-error argument-error () "The argument 'format' must be a literal string!"))
  ;; Evaluate the data once
  (let* ((array-var (gensym)) (result (parseq `(unpack-format ,array-var) format)))
    (unless result
      (f-error argument-error () "Invalid format string!"))
    (destructuring-bind (bytes code) result
      `(let ((,array-var ,data))
         (unless (= (length ,array-var) ,bytes)
           (f-error argument-error () "Invalid number of bytes. Expected: ~a" ,bytes))
         ;; Generate the code for unpacking using the parseq rule
         (list ,@code)))))

;; Define the pack macro
(defmacro pack (format value-list)
  ;; Check the format string
  (unless (stringp format)
    (f-error argument-error () "The argument 'format' must be a literal string!"))
  (let ((values (gensym)) (result (parseq `pack-format format)))
    (unless result
      (f-error argument-error () "Invalid format string!"))
    (destructuring-bind (bytes code) result
      `(let ((,values ,value-list))
         (when (/= (length ,values) ,(length code))
           (f-error argument-error () "Invalid number of values. Expected: ~a" ,(length code)))
         (destructuring-bind ,(mapcar #'first code) ,values
           ;; Convert values and check limits
           ,@(loop for c in code for i upfrom 0 collect `(setf ,(first c) ,(second c)))
           (make-array ,bytes :element-type '(unsigned-byte 8) :initial-contents (list ,@(loop for c in code append (third c)))))))))
