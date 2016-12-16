(in-package :gloss.vao)

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
    (:fixed '(integer 0 65535)) ;; 16.16 fixed point integer in 32-bits of space
    (:half-float '(integer 0 65535)) ;; half float in 16 bits of space.
    (:double 'double-float)))

(defun gl-type->byte-size (gl-type)
  (ecase gl-type
    (:float 4)
    (:byte 1)
    (:unsigned-byte 1)
    (:short 2)
    (:unsigned-short 2)
    (:int 4)
    (:unsigned-int 4)
    (:fixed 2) ;; 16.16 fixed point integer in 32-bits of space
    (:half-float 2) ;; half float in 16 bits of space.
    (:double 8)))


(defun allocate-gl-typed-static-vector (len gl-type)
  (static-vectors:make-static-vector
   len :element-type (gl-type->cl-type gl-type)))

(defun vec->sv/unsigned-byte
    (gl-type out-svec write-byte-index in-vec read-element-index num-read-elems)
  "Read a complete attribute whose component type is intended to be
the GL-TYPE from (the CL vector/array) in-vec at
read-element-index (one index per component) then convert each
component to an unsigned-byte representation and write it into the
static-vector out-svec at the write-byte-index location.  Return the
values of out-svec and the number of bytes written."
  (let ((gl-type-byte-size (gl-type->byte-size gl-type)))

    (loop
       ;; the function used to encode the value into whatever we need.
       :with encoder = (cond
                         ((eq gl-type :float) #'ieee-floats::encode-float32)
                         (t #'identity))
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
