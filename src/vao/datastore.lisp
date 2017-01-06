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
(defclass attribute-descriptor ()
  (;; A reference to the attribute in the attribute set.
   (%attr :initarg :attr
          :initform NIL
          :accessor attr)
   ;; How long is the raw representation of the attribute entry (including all
   ;; of its components) in bytes?
   (%raw-byte-length :initarg :raw-byte-length
                     :initform 0
                     :accessor raw-byte-length)
   ;; What is the byte length of the fully alignable attribute?
   (%aligned-byte-length :initarg :aligned-byte-length
                         :initform NIL
                         :accessor aligned-byte-length)
   ;; In the datastore, what is the byte offset to the first attribute in the
   ;; native data array?
   (%offset :initarg :offset
            :initform 0
            :accessor offset)
   ;; What is the stride to the next attribute entry?
   (%stride :initarg :stride
            :initform 0
            :accessor stride)
   ;; How many valid entries have we stored in the data store?
   (%num-valid-attributes :initarg :num-valid-attributes
                          :initform 0
                          :accessor num-valid-attributes)
   ;; When we need to append the next attribute into the data store, what is
   ;; the byte index at which we need to write it?
   (%byte-write-index :initarg :byte-write-index
                      :initform 0
                      :accessor byte-write-index)))

;; A datastore is responsible for ONE native array of attribute data.
(defclass datastore ()
  ;; To what should we align the start of each attribute?
  ;; This is the same for all attributes in the datastore.
  ((%force-alignment-p :initarg :force-alignment-p
                       :initform T
                       :accessor force-alignment-p)
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

(defun align-up-to (value power)
  "Align the value up to the next (expt 2 POWER) multiple if required."
  (let ((align (expt 2 power)))
    (logand (+ value (1- align)) (lognot (1- align)))))

(defun next-multiple (value multiple)
  (* (ceiling (/ value multiple)) multiple))

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
    (:fixed 'single-float) ;; 16.16 fixed point integer
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
precision form of it. This is really a sign-extension operation."
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


(defun test-0 ()
  "Test vec->static-vector/unsigned-byte."
  (let ((sv (allocate-gl-typed-static-vector 32 :unsigned-byte))
        (tests `( ;; gl-type write-index in-vec read-index
                 (:unsigned-byte 0 ,(vector 0 1 2 3 255) 0)
                 (:byte 0 ,(vector -2 -1 0 1 2) 0)
                 (:unsigned-short 0 ,(vector 0 1 2 65534 65535) 0)
                 (:short 0 ,(vector -32768 -32767 -1 0 1 32766 32767) 0)
                 (:int 0 ,(vector -2147483648 -1 0 1 2147483647) 0)
                 (:unsigned-int 0 ,(vector 0 1 2 4294967295) 0)
                 (:fixed 0 ,(vector -2.0 -1.5 -1.0 0 1.0 1.5 2.0) 0)
                 (:half-float 0 ,(vector -65504.0 -2 1.5 0 1.5 2.0 65504.0) 0)
                 (:float 0 ,(vector -2 -1.5 0 1.5 2.0) 0)))
        (passed 0))

    (flet ((clear (svec)
             ;; clear out-vec...
             (loop :for idx :below (length svec) :do
                (setf (aref svec idx) 0)))

           (stored (gl-type in-vec read-index sv write-index)
             (format t "Stored ~A:~%in-vec = ~A from index ~A~%sv= into ~A~%at byte index ~A~%"
                     gl-type in-vec read-index sv write-index)))

      ;; test each gl-type
      (loop :for (gl-type read-index in-vec write-index) :in tests :do
         (clear sv)
         ;; first, fill it.
         (vec->sv/unsigned-byte gl-type sv write-index in-vec read-index
                                (length in-vec))
         ;; then emit what we did.
         (stored gl-type in-vec read-index sv write-index)
         ;; Then extract it
         (let ((out-vec (sv/unsigned-byte->vec gl-type (length in-vec) sv 0)))

           (format t "out-vec is ~A~%" out-vec)

           ;; and compare
           (cond
             ;; = is not entirely appropriate for for floating point values,
             ;; and definitely not appropriate for :fixed, since :fixed is
             ;; designed to lose precision.
             ((every #'identity (map 'vector #'= in-vec out-vec))
              (format t "gl-type: ~A PASSED~%" gl-type)
              (incf passed))
             (t
              (format t "gl-type: ~A FAILUE~%" gl-type)))

           (format t "~%")))

      (format t "Out of ~A tests, ~A passed.~%" (length tests) passed)


      (static-vectors:free-static-vector sv))))


;; TODO: This returns a datastore to store the data appropriate for
;; the definition of the datastore-name whose native-data type is
;; always unsigned-byte.
(defun make-datastore (datastore-name layout-set)
  ;; 1. Lookup datastore in layout-set.
  (let ((datastore (make-instance 'datastore))
        (named-layout (lookup-named-layout datastore-name layout-set))
        (attr-set (attribute-set layout-set)))

    ;; check what I'm about to work with...
    (format t "named-datastore ~A is ~A~%" datastore-name named-layout)
    (loop :for attr :in (template named-layout) :do
       (format t "Attribute: ~A -> ~A~%" attr (lookup-attribute attr attr-set)))

    ;; 1. Create the attribute-descriptors for this datastore.
    (setf (descriptors datastore)
          (gen-attribute-descriptors (data-format (properties named-layout))
                                     named-layout attr-set))

    datastore))

(defgeneric gen-attribute-descriptors (kind named-layout attr-set))


(defmethod gen-attribute-descriptors ((kind (eql :separate))
                                      named-layout attr-set)
  ;; We only need 1 attribute-descriptor for a :separate datastore since there
  ;; is only 1 attribute to ever worry about.
  (let* ((descriptor (make-instance 'attribute-descriptor))
         (attribute-desc-table (make-hash-table))
         (attr-name (first (template named-layout)))
         (attr (lookup-attribute attr-name attr-set)))

    ;; 0. Set up a reference to the real attribute definition.
    (setf (attr descriptor) attr)

    ;; 1. compute raw attribute length
    (setf (raw-byte-length descriptor)
          (attribute-size attr))

    ;; 2. Compute alignment size, which is the one we really use.
    (setf (aligned-byte-length descriptor)
          (if (align (properties named-layout))
              ;; align up to the next multiple of 4 or the size of the
              ;; component type, whichever is larger.
              (next-multiple (raw-byte-length descriptor)
                             (max 4 (attribute-type-size attr)))
              ;; assume alignment value of 1, so keep raw byte length
              (raw-byte-length descriptor)))

    ;; 3. Compute offset, only one attribute in a :separate layout,
    ;; its offset is 0.
    (setf (offset descriptor) 0)

    ;; 4. Compute Stride (for the whole attribute)
    ;; NOTE: I verified it is ok to use stride even in the case of a tightly
    ;; packed set of attributes. In this case, this is correct if we align
    ;; or not.
    (setf (stride descriptor) (aligned-byte-length descriptor))

    ;; 5. Compute the byte position where we write new entries into the
    ;; native datastore array.
    (setf (byte-write-index descriptor) 0)

    ;; insert the descriptor into the table.
    (setf (gethash attr-name attribute-desc-table) descriptor)

    ;; return the hash table.
    attribute-desc-table))


(defmethod gen-attribute-descriptors ((kind (eql :interleave))
                                      named-layout attr-set)
  nil)


(defmethod gen-attribute-descriptors ((kind (eql :block)) named-layout attr-set)
  nil)


(defun test-1 (&optional (datastore-name 'vertices))
  (make-datastore datastore-name (doit)))
