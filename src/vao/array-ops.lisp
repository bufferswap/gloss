(in-package #:gloss.vao)


(defun align-up-to (value power)
  "Align the value up to the next (expt 2 POWER) multiple if required."
  (let ((align (expt 2 power)))
    (logand (+ value (1- align)) (lognot (1- align)))))

(defun next-multiple (value multiple)
  "Return the integer VALUE if it is easily divisible by MULTIPLE, or the next
highest integer that is divisible by MULTIPLE."
  (* (ceiling (/ value multiple)) multiple))

(defun gl-type->cl-type (gl-type)
  "Convert the GL-TYPE, example :unsigned-byte, to an appropriate Common Lisp
type, such as '(integer 0 255), and return the type specifier form."
  (ecase gl-type
    (:float 'single-float)
    (:byte '(integer -128 127))
    (:unsigned-byte '(integer 0 255))
    (:short '(integer -32768 32767))
    (:unsigned-short '(integer 0 65535))
    (:int '(integer -2147483648 2147483647))
    (:unsigned-int '(integer 0 4294967295))
    ;; This next one isn't completely correct, but it is functionally correct.
    (:fixed 'single-float) ;; 16.16 fixed point integer
    (:half-float '(single-float -65504.0 65504.0)) ;; ieee 754-2008
    (:double 'double-float)))

(defun gl-type->byte-size (gl-type)
  "Following the definitions in the OpenGL Standard, return a byte size for
each GL-TYPE."
  (ecase gl-type
    (:unsigned-byte 1)
    (:byte 1)
    (:unsigned-short 2)
    (:short 2)
    (:unsigned-int 4)
    (:int 4)
    (:fixed 4) ;; 16.16 fixed point integer in 32-bits of space
    (:half-float 2) ;; half float in 16 bits of space.
    (:float 4)
    (:double 8)))

(defun zero-wrt-gl-type (gl-type)
  "Return a zero value appropriate for the given GL-TYPE."
  (ecase gl-type
    ((:unsigned-byte :byte :unsigned-short :short :unsigned-int :int :fixed)
     0)
    ((:half-float :float)
     0.0)
    (:double
     0d0)))

;; TODO: This needs to be broken up into individual functions per the
;; n-bit-width, have hard coded constants added, and declaimed with
;; THE's until it optimizes to a tiny function.
(declaim (inline decode-uN->sN))
(defun decode-uN->sN (val n-bit-width)
  "Slowly decode a twos complement number encoded into the unsigned
positive number VAL back into the signed common lisp infinite
precision form of it. This is really a sign-extension operation."
  (logior (* (ldb (byte 1 (1- n-bit-width)) val)
             (- (expt 2 n-bit-width)))
          val))

(defun allocate-gl-typed-static-vector (len gl-type)
  "Create a static-vectors array of the required length LEN and GL-TYPE.
The returned vector's element typ is actually the result of calling
GL-TYPE->CL-TYPE on GL-TYPE."
  (static-vectors:make-static-vector
   len
   :element-type (gl-type->cl-type gl-type)
   :initial-element (zero-wrt-gl-type gl-type)))


(defgeneric coerce-harder (value fl-type-target)
  (:documentation
   "Coerce the VALUE to the GL-TYPE-TARGET. In the case of VALUE being
a floating point number and the GL-TYPE-TARGET specifying a integral
quantity, this can lose precision. Note, that if you're coercing a value
that is larger than the type to which you're coercing, you will clamp to
the edge values of the type in question."))

(defmethod coerce-harder (value gl-type-target)
  (coerce value (gl-type->cl-type gl-type-target)))


(defun vec->sv/unsigned-byte
    (gl-type out-svec write-byte-index in-vec read-element-index num-read-elems)
  "Read a NUM-READ-ELEMS of data from IN-VEC at READ-ELEMENT-INDEX
that is intended to be GL-TYPE data and store in a native little
endian unsigned-byte representation in OUT-VEC starting at
WRITE-BYTE-INDEX. The caller should guarantee that this will validly
reference the arrays through all indicies. Return the values OUT-SVEC
and the number of bytes written to OUT-SVEC."
  (let ((gl-type-byte-size (gl-type->byte-size gl-type)))

    (loop
       ;; the function used to encode the value into whatever we need.
       :with encoder = (case gl-type
                         (:float #'ieee-floats::encode-float32)
                         (:fixed (lambda (val)
                                   (truncate (round (* val (expt 2 16))))))
                         (:half-float #'ieee-floats::encode-float16)
                         (otherwise #'identity))
       ;; This is little endian.
       :with endian4 = (vector (byte 8 0) (byte 8 8) (byte 8 16) (byte 8 24))
       ;; how many bytes we totally write
       :with write-count = 0

       ;; iterate over each in-vec element in the right slice of the array.
       :for read-index
       :from read-element-index
       :below (+ read-element-index num-read-elems)

       ;; get the value we need, always convert.
       :for value = (coerce-harder (aref in-vec read-index) gl-type)
       ;; convert it to an unsigned int representation.
       :for converted-value = (funcall encoder value)

       ;; cut it into bytes and store it into the out-vec
       :do
       (loop :for byte-offset :below gl-type-byte-size :do
          ;; we're writing into an unsigned-byte static vector, so we
          ;; don't need to worry about writing alignment.
          (setf (aref out-svec (+ write-byte-index write-count))
                (ldb (aref endian4 byte-offset) converted-value))
          (incf write-count)))

    (values out-svec (* gl-type-byte-size num-read-elems))))


;; TODO, needs a lot of testing.
(defun sv/unsigned-byte->vec (gl-type num-gl-elems in-svec read-sv-index
                              &key (out-vec NIL) (write-elem-index 0))
  "Starting at the READ-SV-INDEX byte in the static vector IN-SVEC,
extract NUM-GL-ELEMS of type GL-TYPE. If keyword argument :OUT-VEC is
a CL single dimensional array or vector, then write the extracted
elements to there starting at :WRITE-ELEM-INDEX, which defaults to
0. Otherwise allocate a new array of NUM-GL-ELEMS whose size will
be (+ WRITE-ELEM-INDEX NUM-GL-ELEMS), fill the array with the data,
and return that. Return OUT-VEC and the number of bytes read from
IN-SVEC."
  (let ((gl-type-byte-size (gl-type->byte-size gl-type))
        (out-vec (if out-vec out-vec (make-array (+ write-elem-index
                                                    num-gl-elems)))))
    (loop
       ;; how many bytes I've processed from in-svec
       :with read-count = 0
       ;; This is little endian.
       :with endian4 = (vector (byte 8 0) (byte 8 8) (byte 8 16) (byte 8 24))
       ;; stuff to decode the integers I made into real things.
       :with decoder = (case gl-type
                         (:float #'ieee-floats::decode-float32)
                         (:fixed (lambda (x) (float (/ (decode-uN->sN x 32)
                                                       (expt 2 16)))))
                         (:byte (lambda (x) (decode-uN->sN x 8)))
                         (:short (lambda (x) (decode-uN->sN x 16)))
                         (:int (lambda (x) (decode-uN->sN x 32)))
                         (:half-float #'ieee-floats::decode-float16)
                         (otherwise #'identity))

       ;; create each data entry into the appropriate spot in out-vec
       :for write-idx :from write-elem-index :below (+ num-gl-elems
                                                       write-elem-index)

       ;; Extract the unsigned bytes, condense into a larger integer based upon
       ;; the number of bytes required for the gl-type.
       :for value = (loop
                       :with num = 0

                       :for read-idx
                       :from (+ read-sv-index read-count)
                       :below (+ read-sv-index read-count gl-type-byte-size)

                       :for endian-idx :from 0 :by 1
                       :finally (return num)
                       :do
                       (setf num
                             (dpb (aref in-svec read-idx)
                                  (aref endian4 endian-idx)
                                  num))
                       (incf read-count))

       :do
       ;; and now decode the value and store it in out-svec at write-elem-index
       (setf (aref out-vec write-idx) (funcall decoder value)))
    out-vec))
