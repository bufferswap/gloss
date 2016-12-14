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

;; Here for reference for me.
#++(defun encode-and-explode (result value value-gl-type &key (align 1))
     (let ((endian2 (vector (byte 8 0) (byte 8 8)))
           (endian4 (vector (byte 8 0) (byte 8 8) (byte 8 16) (byte 8 24))))
       (ecase value-gl-type
         (:float
          (let ((val (ieee-floats::encode-float32 value)))
            (psetf (aref result 0) (ldb (aref endian4 0) val)
                   (aref result 1) (ldb (aref endian4 1) val)
                   (aref result 2) (ldb (aref endian4 2) val)
                   (aref result 3) (ldb (aref endian4 3) val))
            ;; handle alignment
            (cond
              ((or (= align 1) (= align 2) (= align 4))
               ;; we're already aligned to these values
               (values result 4))
              ((= align 8)
               (psetf (aref result 4) 0
                      (aref result 5) 0
                      (aref result 6) 0
                      (aref result 7) 0)
               (values result 8))
              (t
               (error "encode-and-explode: bad alignment: ~A" align)))))

         ((:byte :unsigned-byte)
          (setf (aref result 0) value)
          ;; handle alignment
          (cond
            ((= align 1)
             (values result 1))
            ((= align 2)
             (psetf (aref result 1) 0)
             (values result 2))
            ((= align 4)
             (psetf (aref result 1) 0
                    (aref result 2) 0
                    (aref result 3) 0)
             (values result 4))
            ((= align 8)
             (psetf (aref result 1) 0
                    (aref result 2) 0
                    (aref result 3) 0
                    (aref result 4) 0
                    (aref result 5) 0
                    (aref result 6) 0
                    (aref result 7) 0)
             (values result 8))
            (t
             (error "encode-and-explode: bad alignment: ~A" align))))

         ((:short :unsigned-short :fixed)
          (psetf (aref result 0) (ldb (aref endian2 0) value)
                 (aref result 1) (ldb (aref endian2 1) value))
          ;; handle alignment
          (cond
            ((or (= align 1) (= align 2))
             (values result 2))
            ((= align 4)
             (psetf (aref result 2) 0
                    (aref result 3) 0)
             (values result 4))
            ((= align 8)
             (psetf (aref result 2) 0
                    (aref result 3) 0
                    (aref result 4) 0
                    (aref result 5) 0
                    (aref result 6) 0
                    (aref result 7) 0)
             (values result 8))
            (t
             (error "encode-and-explode: bad alignment: ~A" align))))

         ((:int :unsigned-int)
          (psetf (aref result 0) (ldb (aref endian4 0) value)
                 (aref result 1) (ldb (aref endian4 1) value)
                 (aref result 2) (ldb (aref endian4 2) value)
                 (aref result 3) (ldb (aref endian4 3) value))
          ;; handle alignment
          (cond
            ((or (= align 1) (= align 2) (= align 4))
             ;; we're already aligned to these values
             (values result 4))
            ((= align 8)
             (psetf (aref result 4) 0
                    (aref result 5) 0
                    (aref result 6) 0
                    (aref result 7) 0)
             (values result 8))
            (t
             (error "encode-and-explode: bad alignment: ~A" align))))

         (:half-float
          (error "encode-and-explode :half-float NIY"))

         (:double
          (error "encode-and-explode :double NIY")))))

(defun gl-type->cl-type (gl-type)
  (ecase gl-type
    (:float 'single-float)
    (:byte '(integer -128 127))
    (:unsigned-byte '(integer 0 255))
    (:short '(integer -32768 32767))
    (:unsigned-short '(integer 0 65535))
    (:int '(integer −2147483648 2147483647))
    (:unsigned-int '(integer 0 4294967295))
    (:fixed '(integer 0 65535)) ;; 16.16 fixed point integer in 32-bits of space
    (:half-float '(integer 0 65535)) ;; half float in 16 bits of space.
    (:double 'double-float)))


(defun float->static-vector/unsigned-byte
    (out-svec write-byte-index in-vec read-element-index num-read-elems)
  "Read a complete attribute whose component type is intended to be
the GL type :float from (the CL vector/array) in-vec at
read-element-index (one index per component) then convert each
component to a byte representation and write it into the static-vector
out-svec at the write-byte-index location.  Return the values of
out-svec and the number of bytes written."

  (loop
     ;; This is little endian.
     :with endian4 = (vector (byte 8 0) (byte 8 8) (byte 8 16) (byte 8 24))
     ;; how many bytes we totally write
     :with write-count = 0

     ;; iterate over each in-vec element in the right slice of the array.
     :for read-index
     :from read-element-index
     :below (+ read-element-index num-read-elems)

     ;; get the value we need, always convert.
     :for value = (coerce (aref in-vec read-index) 'single-float)
     ;; convert it to an unsigned int representation.
     :for ieee-uint = (ieee-floats::encode-float32 value)

     ;; cut it into bytes and store it into the out-vec
     :do
     (loop :for byte-offset :below 4 :do
        ;; we're writing into an unsigned-byte static vector, so we
        ;; don't need to worry about writing alignment.
        (setf (aref out-svec (+ write-byte-index write-count))
              (ldb (aref endian4 byte-offset) ieee-uint))
        (incf write-count)))

  (values out-svec (* 4 num-read-elems)))

(defun allocate-gl-typed-static-vector (len gl-type)
  (static-vectors:make-static-vector
   len :element-type (gl-type->cl-type gl-type)))

(defun test-0 ()
  "Test attr/gl-float->static-vector/unsigned-byte"
  (let* ((out-vec-len 32)
         (out-vec (allocate-gl-typed-static-vector out-vec-len :unsigned-byte))
         (write-byte-index 0)
         (in-vec (vector 100 455/222 54.6798))
         (read-element-index 0)
         (num-read-elems (length in-vec)))

    ;; clear out-vec...
    (loop :for idx :below out-vec-len :do
       (setf (aref out-vec idx) 0))

    ;; rip out the floats and store in the out-vec.
    (float->static-vector/unsigned-byte
     out-vec write-byte-index in-vec read-element-index num-read-elems)

    (format t "Stored:~%in-vec = ~A~%out-vec= into ~A~%at byte index ~A~%"
            in-vec out-vec write-byte-index)

    (static-vectors:free-static-vector out-vec)))


(defun float->static-vector/unsigned-int
    (out-svec write-byte-index in-vec read-element-index num-read-elems)
  "Read a complete attribute whose component type is intended to be
the GL type :float from (the CL vector/array) in-vec at
read-element-index (one index per component) then convert each
component to a byte representation and write it into the static-vector
out-svec at the write-byte-index location.  Return the values of
out-svec and the number of bytes written."

  (values out-svec 0))
