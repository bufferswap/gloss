(in-package :gloss.vao)

;;;; NOTE: The gl-type :fixed is not well supported in this
;;;; library. In order to support it properly, conversion functions to
;;;; and from 16.16 fixed point have to be created. There is a "fixed"
;;;; CL library, but I don't know how to use it (cause I haven't
;;;; looked).

;; To support gl-type :half-float
(declaim (inline ieee-floats::encode-float16 ieee-floats::decode-float16))
(ieee-floats::make-float-converters ieee-floats::encode-float16
                                    ieee-floats::decode-float16
                                    5 10 nil)

;; In a datastore, we record, for each attribute participating,
;; information about how that attribute is exactly layed out into the
;; datastore, how many entries of that attribute there are, etc, etc, etc.
(defstruct attribute-descriptor
  ;; How long is the raw representation of the attribute entry (including all
  ;; of its components) in bytes?
  raw-byte-length
  ;; What is the byte length of the fully alignable attribute?
  aligned-byte-length
  ;; In the datastore, what is the byte offset to the first attribute in the
  ;; native data array?
  offset
  ;; What is the stride to the next attribute entry?
  stride
  ;; How many valid entries have we stored in the data store?
  num-valid-attributes
  ;; When we need to write the next attribute intot he data store, what is
  ;; the byte index at which we need to write it?
  byte-write-index)

;; A datastore is responsible for ONE native array of attribute data.
(defclass datastore ()
  ;; To what should we align the start of each attribute?
  ;; This is the same for all attributes in the datastore.
  ((%force-alignment-p :initarg :force-alignment-p
                       :initform T
                       :accessor force-alignment-p)
   ;; What is the attribute alignment number?
   (%alignment :initarg :alignment
               :initform 1
               :accessor alignment)
   ;; The actual static-vector storage for the attribute data.
   (%native-data :initarg :native-data
                 :initform NIL
                 :accessor native-data)
   ;; The raw type of the static-vector. We sometimes need to know this before
   ;; having an actual native-data array.
   (%native-type :initarg :native-type
                 :initform NIL
                 :accessor native-type)
   ;; A hash table keyed by attribute shortname and whose value is
   ;; an attribute-descriptor structure.
   (%descriptors :initarg :descriptors
                 :initform (make-hash-table)
                 :accessor descriptors)))

(defun make-datastore (datastore-name layout-set &key (force-align-p T))
  ;; 1. Lookup datastore in layout-set.
  ;; 2. Looking at the component types of all of the attributes:
  ;; 2a. Determine the best native-type that leads to the least memory copying.
  ;; 2b. Compute and store appropriate alignment-descriptors.
  ;; 3. TODO
  nil)

(defun gl-type->cl-type (gl-type)
  (ecase gl-type
    (:float 'single-float)
    (:byte '(integer -128 127))
    (:unsigned-byte '(integer 0 255))
    (:short '(integer -32768 32767))
    (:unsigned-short '(integer 0 65535))
    (:int '(integer -2147483648 2147483647))
    (:unsigned-int '(integer 0 4294967295))
    ;; This next one isn't completely correct, but it is functionally correct.
    (:fixed '(integer 0 4294967295)) ;; 16.16 fixed point integer in 32-bits
    (:half-float '(single-float -65504.0 65504.0)) ;; ieee 754-2008
    (:double 'double-float)))

(defun gl-type->byte-size (gl-type)
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



;; TODO: This needs to be broken up into individual functions per the
;; n-bit-width, have hard coded constants added, and declaimed with
;; THE's until it optimizes to a tiny function.
(declaim (inline decode-uN->sN))
(defun decode-uN->sN (val n-bit-width)
  "Slowly decode a twos complement number encoded into the unsigned
positive number VAL back into the signed common lisp infinite
precision form of it."
  (logior (* (ldb (byte 1 (1- n-bit-width)) val)
	     (- (expt 2 n-bit-width)))
	  val))


(defun allocate-gl-typed-static-vector (len gl-type)
  (static-vectors:make-static-vector
   len :element-type (gl-type->cl-type gl-type)))


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
       :for value = (coerce (aref in-vec read-index) (gl-type->cl-type gl-type))
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


(defun test-3 ()
  "Test vec->static-vector/unsigned-byte."
  (let ((out-vec (allocate-gl-typed-static-vector 32 :unsigned-byte))
        (tests `( ;; gl-type write-index in-vec read-index
                 (:unsigned-byte 0 ,(vector 0 1 2 3 255) 0)
                 (:byte 0 ,(vector -2 -1 0 1 2) 0)
                 (:unsigned-short 0 ,(vector 0 1 2 65534 65535) 0)
                 (:short 0 ,(vector -32768 -32767 -1 0 1 32766 32767) 0)
                 (:int 0 ,(vector -2147483648 -1 0 1 2147483647) 0)
                 (:unsigned-int 0 ,(vector 0 1 2 4294967295) 0)
                 (:fixed 0 ,(vector #x00010000 #x00020000 #x00030000) 0)
                 (:half-float 0 ,(vector -65504.0 -2 1.5 0 1.5 2.0 65504.0) 0)
                 (:float 0 ,(vector -2 -1.5 0 1.5 2.0) 0))))

    (flet ((clear (sv)
             ;; clear out-vec...
             (loop :for idx :below (length sv) :do
                (setf (aref sv idx) 0)))

           (stored (gl-type in-vec read-index out-vec write-index)
             (format t "Stored ~A:~%in-vec = ~A from index ~A~%out-vec= into ~A~%at byte index ~A~%~%"
                     gl-type in-vec read-index out-vec write-index)))

      ;; test each gl-type
      (loop :for (gl-type read-index in-vec write-index) :in tests :do
         (clear out-vec)
         (vec->sv/unsigned-byte gl-type out-vec write-index in-vec read-index
                                (length in-vec))
         (stored gl-type in-vec read-index out-vec write-index))


      (static-vectors:free-static-vector out-vec))))
