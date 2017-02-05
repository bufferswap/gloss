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


;; This decribes how a attribute is layed out in a generalized array.
;; This specific class is suitable for picking attributes out of incoming
;; datra arrays which are usually regular CL arrays of ARRAY or VECTOR type
;; such as MAKE-ARRAY, VECTOR, or #().
;;
;; NOTE: offset and stride are in ELEMENTS in this class instance.
(defclass attribute-descriptor ()
  (;; A reference to the attribute in the attribute set for which we built
   ;; this descriptor.
   (%attr :initarg :attr
          :initform NIL
          :accessor attr)
   ;; In the datastore, what is the offset to the first attribute in the
   ;; data array?
   (%offset :initarg :offset
            :initform 0
            :accessor offset)
   ;; What is the stride to the next attribute entry?
   (%stride :initarg :stride
            :initform 0
            :accessor stride)
   ;; This is the attribute index into which we should start
   ;; appending. This is always 1+ the maximum index the user may have
   ;; setf an attribute into. Consequently, this is also considered the
   ;; "length" of the datastore, which is the highest defined attribute
   ;; (which could be less than or equal to the SIZE of the datastore).
   (%appending-index :initarg :appending-index
                     :initform 0
                     :accessor appending-index)))

;; However, in the data representation of the attributes, we need more
;; specific information about exact byte layouts, byte lengths, and such,
;; so we derive an attribute-descriptor to further specialize the kind of
;; attribute-descriptor we need for data storage in a datastore.
;;
;; NOTE: offset and stride are in BYTES in this class instance.
(defclass native-attribute-descriptor (attribute-descriptor)
  (;; How long is the raw representation of the attribute entry (including all
   ;; of its components) in bytes?
   ;; NOTE: Maybe this should just be a call to (attribute-size attr).
   (%raw-byte-length :initarg :raw-byte-length
                     :initform 0
                     :accessor raw-byte-length)
   ;; What is the byte length of the fully alignable attribute?
   (%aligned-byte-length :initarg :aligned-byte-length
                         :initform NIL
                         :accessor aligned-byte-length)
   ))

(defmethod num-attrs ((attr-desc attribute-descriptor))
  "Return the number of attributes that are considered defined by this
ATTR-DESC. In practice this means one more than the highest index stored
into the datastore with which this ATTR-DESC is associated."
  (appending-index attr-desc))

;; A datastore is responsible for representing the layout of all attributes
;; in an array. The base data store class can represent CL arrays. This is
;; used to manage incoming data arrays.
(defclass datastore ()
  (
   ;; This is the name of the named-layout in the layout-set for which
   ;; this data store was constructed.
   (%name :initarg :name
          :initform NIL
          :accessor name)
   (%layout-set :initarg :layout-set
                :initform NIL
                :accessor layout-set)
   ;; This is how many template group instances may be stored in the native
   ;; data array.
   ;;
   ;; Example:
   ;; If the datastore is :interleave and the
   ;; template group (position normal uv), then a size of
   ;; 16 means there will be 16 (position normal uv) groups in the array.
   ;; This also means there are 16 positions, 16 normals, and 16 uvs.
   (%size :initarg :size
          :initform 0
          :accessor size)

   ;; The actual storage for the attribute data. Is often a static-vectors.
   (%data :initarg :data
          :initform NIL
          :accessor data)
   ;; The raw type of the data vector. We sometimes need to know this before
   ;; having an actual data array.
   (%data-type :initarg :data-type
               :initform NIL
               :accessor data-type)
   ;; A hash table keyed by attribute shortname and whose value is
   ;; a native-attribute-descriptor structure.
   (%descriptors :initarg :descriptors
                 :initform (make-hash-table)
                 :accessor descriptors)
   ;; Is this datastore resizeable?
   (%resizeable-p :initarg :resizeable-p
                  :initform NIL
                  ;; Once set in the constructor, this cannot be altered.
                  :reader resizeable-p)
   ))

;; This derived type of datastore has methods on it to understand the
;; fact that this type represents binary level attribute layouts in
;; native machine memory arrays (such as static-vectors) which are
;; suitable for upload to the GPU.
(defclass native-datastore (datastore) ())

(defmethod named-layout ((ds datastore))
  (lookup-named-layout (name ds) (layout-set ds)))

(defmethod attr-set ((ds datastore))
  (attribute-set (layout-set ds)))

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
  (static-vectors:make-static-vector
   len
   :element-type (gl-type->cl-type gl-type)
   :initial-element (zero-wrt-gl-type gl-type)))

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

;; TODO: This returns a datastore to store the data appropriate for
;; the definition of the datastore-name whose data type is
;; always unsigned-byte.
(defun make-native-datastore (datastore-name layout-set
                              &key (size 4) (resizeable-p NIL))
  (let* ((datastore (make-instance 'native-datastore
                                   :name datastore-name
                                   :layout-set layout-set
                                   :size size
                                   :resizeable-p resizeable-p))
         (named-layout (named-layout datastore))
         (attr-set (attr-set datastore)))

    ;; DEBUG: check what I'm about to work with...
    (format t "named-datastore ~A is ~A~%" datastore-name named-layout)
    (loop :for attr :in (template named-layout) :do
       (format t "Attribute: ~A -> ~A~%" attr (lookup-attribute attr attr-set)))

    ;; 1. Create the attribute-descriptors for this datastore.
    (gen-attribute-descriptors datastore (data-format
                                          (properties named-layout)))

    ;; 2. Allocate the native array.
    (gen-data-storage datastore (data-format (properties named-layout)))

    datastore))

(defmethod destroy-datastore (ds)
  (setf (data ds) NIL))

(defmethod destroy-datastore ((ds native-datastore))
  (when (data ds)
    (static-vectors:free-static-vector (data ds))
    (setf (data ds) NIL)))




(defun compute-attr-alignment (attr named-layout)
  "Compute the aligned size of the ATTR using either the alignment size of the
component or 4 (which ever is larger). The result is a number in which the
ATTR will fit such that the components pack tightly and there is padding at
the end of the ATTR upto the aligned size."
  ;; attr is a defstruct of the attr from the attr-set
  (let ((attr-byte-len (attribute-size attr))
        (attr-type-size (attribute-type-size attr)))
    (if (align (properties named-layout))
        ;; align up to the next multiple of 4 or the size of the
        ;; component type, whichever is larger.
        (next-multiple attr-byte-len (max 4 attr-type-size))
        ;; assume alignment value of 1, so keep raw byte length
        attr-byte-len)))








(defgeneric compute-data-size-in-bytes (ds kind)
  (:documentation "Compute the total size, in bytes, of a native data array
which will be used to store (possibly) aligned attribute data."))

(defmethod compute-data-size-in-bytes ((ds native-datastore)
                                       (kind (eql :interleave)))
  (* (size ds) ;; The number of attribute groups....

     ;; Compute the size of one "group" (which is the total size of
     ;; all aligned attributes that can be referenced by the same
     ;; index in the datastore)
     (loop
        :for d :being :the :hash-values :in (descriptors ds)
        :summing (aligned-byte-length d))))

(defmethod compute-data-size-in-bytes ((ds native-datastore)
                                       (kind (eql :separate)))
  ;; This is the same algorithm as for :interleave for this function.
  ;; But there is only a single attribute in the attribute group.
  (compute-data-size-in-bytes ds :interleave))

(defmethod compute-data-size-in-bytes ((ds native-datastore)
                                       (kind (eql :block)))
  (error "compute-data-size-in-bytes :block not implemented!"))





(defgeneric gen-data-storage (ds kind)
  (:documentation "Determine what underlying type and how big to allocate
the storage for datastroe DS of KIND. Often the storage will be a
static-vectors type, but it can be other more complex things too, depending
upon the circumstances."))


(defmethod gen-data-storage ((ds native-datastore) (kind (eql :interleave)))
  (let ((data-size-in-bytes (compute-data-size-in-bytes ds kind)))

    (format t "Allocating data :unsigned-byte size of: ~A~%"
            data-size-in-bytes)

    ;; 3. Fix up rest of datastore object.
    (setf (data-type ds) :unsigned-byte ;; the GL type...

          (data ds) (allocate-gl-typed-static-vector data-size-in-bytes
                                                     (data-type ds))))
  ds)

(defmethod gen-data-storage ((ds native-datastore) (kind (eql :separate)))
  ;; This is the same algorithm as :interelave for this function.
  ;; But there is only a single attribute in the attribute group.
  (gen-data-storage ds :interleave))

(defmethod gen-data-storage ((ds native-datastore) (kind (eql :block)))
  (error "gen-data-storage for :block unimplemented"))














(defgeneric gen-attribute-descriptors (ds kind)
  (:documentation "Create and store into datastore DS of KIND a collection
of attribute descriptors for which the DS is responsible for maintaing the
storage."))

(defmethod gen-attribute-descriptors ((ds native-datastore)
                                      (kind (eql :interleave)))
  (let* ((named-layout (named-layout ds))
         (attr-set (attr-set ds))
         (attribute-desc-table (make-hash-table))
         ;; 1. assemble the attribute-descriptors in order in the template
         ;; with any easy stuff we can calculate right away.
         (descriptors
          (mapcar (lambda (attr-name)
                    (let* ((attr (lookup-attribute attr-name attr-set)))
                      (make-instance
                       'native-attribute-descriptor
                       :attr attr
                       :raw-byte-length (attribute-size attr)
                       :aligned-byte-length (compute-attr-alignment
                                             attr
                                             named-layout))))

                  (template named-layout))))

    ;; 2. compute offsets and stride for each attribute-descriptor
    (loop
       :with offset = 0
       :with stride = (reduce #'+ descriptors
                              :key (lambda (dsc) (aligned-byte-length dsc)))
       :for desc :in descriptors :do
       ;; compute offset
       (setf (offset desc) offset)
       ;; compute stride
       (setf (stride desc) stride)
       ;; and increment the offset for the next attribute.
       (incf offset (aligned-byte-length desc)))

    ;; 3. Insert all descriptors into hash table
    (loop
       :for desc :in descriptors
       :for attr-name :in (template named-layout) :do
       (setf (gethash attr-name attribute-desc-table) desc))

    ;; Then, set the hash table into the datastore.
    (setf (descriptors ds) attribute-desc-table)))

(defmethod gen-attribute-descriptors ((ds native-datastore)
                                      (kind (eql :separate)))
  ;; We only need 1 attribute-descriptor for a :separate datastore since there
  ;; is only 1 attribute to ever worry about.
  (let* ((named-layout (named-layout ds))
         (attr-set (attr-set ds))
         (attribute-desc-table (make-hash-table))
         (attr-name (first (template named-layout)))
         (attr (lookup-attribute attr-name attr-set))
         (attr-byte-len (attribute-size attr))
         (aligned-attr-byte-len (compute-attr-alignment attr named-layout)))

    (setf (gethash attr-name attribute-desc-table)
          (make-instance 'native-attribute-descriptor
                         :attr attr
                         :raw-byte-length attr-byte-len
                         :aligned-byte-length aligned-attr-byte-len
                         :appending-index 0
                         :offset 0
                         :stride aligned-attr-byte-len))

    ;; Then, set the hash table into the datastore.
    (setf (descriptors ds) attribute-desc-table)))


;; We do different things depending if the datastore is resizeable.
(defmethod gen-attribute-descriptors ((ds native-datastore)
                                      (kind (eql :block)))

  (let* ((named-layout (named-layout ds))
         (attr-set (attr-set ds))
         (attribute-desc-table (make-hash-table))
         ;; 1. assemble the attribute-descriptors in order in the template
         ;; with any easy stuff we can calculate right away.
         (descriptors
          (mapcar (lambda (attr-name)
                    (let* ((attr (lookup-attribute attr-name attr-set)))
                      (make-instance
                       'native-attribute-descriptor
                       :attr attr
                       :raw-byte-length (attribute-size attr)
                       :aligned-byte-length (compute-attr-alignment
                                             attr
                                             named-layout))))

                  (template named-layout))))

    (cond
      ((resizeable-p ds)
       nil)
      (t
       nil))

    ;; Then, set the hash table into the datastore.
    (setf (descriptors ds) attribute-desc-table)))


;; Resize a native data store and ensure the new data is properly maintained.
(defmethod resize ((ds native-datastore))
  ;; XXX TODO Fix me for resizing when in a rezizeable :block situation!!!!

  (let* (;; byte size of the descriptors in toto
         (attr-group-size (loop
                             :for d :being :the :hash-values
                             :in (descriptors ds)
                             :summing (aligned-byte-length d)))
         ;; Compute new number of attributes in the data store
         ;; given the old size
         (new-size (if (zerop (size ds))
                       1024
                       (ceiling (* (size ds) 1.5))))
         ;; allocate the new array.
         (new-data
          (allocate-gl-typed-static-vector
           (* new-size attr-group-size) (data-type ds))))

    (format t "Resizing native data array from ~A attr groups to ~A attr groups.~%"
            (size ds) new-size)

    (unless (zerop (size ds))
      ;; Copy the old data into the new data (as 'unsigned-byte).
      ;; Allocation fills the array already with zero data, so we don't have
      ;; to manage dealing with the unused places.
      (replace new-data (data ds))
      ;; free the native array!
      (static-vectors:free-static-vector (data ds)))

    ;; reset the object to the new stuff.
    (setf (data ds) new-data
          (size ds) new-size)

    ds))

(defmethod attr-ref ((ds native-datastore) name index &rest components)
  ;; 1. Find the attribute descriptor for NAME.
  (let ((attr-desc (gethash name (descriptors ds))))

    (unless attr-desc
      (error
       "ATTR-REF: Non existent attrbute name: ~A in datastore descriptors ~A"
       name (descriptors ds)))

    ;; 2. Find the start byte of the attribute at the specified index.
    (let ((attr-start-byte-idx (+ (offset attr-desc)
                                  (* index (stride attr-desc)))))

      ;; 3. Analyze components, and return the required quantity.
      (cond
        ((null components)
         ;; A. No components specified, return all attribute components as
         ;; a cl array.
         (let* ((num-components (attr-count (attr attr-desc)))
                (result (make-array num-components)))

           ;; B. Extract the components
           (sv/unsigned-byte->vec (attr-type (attr attr-desc))
                                  num-components
                                  (data ds)
                                  attr-start-byte-idx
                                  :out-vec result)
           result))

        (components
         ;; B. There are specific components being referenced.
         (let ((result (make-array (length components) :initial-element 0)))

           ;; Store the components in the order specified into the result.
           (loop
              :for component :in components
              :for result-write-idx :by 1 :do
              (let ((component-index
                     (if (numberp component)
                         component
                         (position component
                                   (attr-accessors (attr attr-desc))))))

                (sv/unsigned-byte->vec
                 (attr-type (attr attr-desc))
                 1 ;; extract them one at a time since they may not be contig.
                 (data ds)
                 (+ attr-start-byte-idx
                    (* component-index
                       (gl-type->byte-size (attr-type (attr attr-desc)))))
                 :out-vec result
                 :write-elem-index result-write-idx)))
           result))))))



(defmethod (setf attr-ref) (comp-vec (ds native-datastore) name index
                            &rest components)
  ;; 1. Find the attribute descriptor for NAME.
  (let ((attr-desc (gethash name (descriptors ds))))

    (unless attr-desc
      (error
       "SETF ATTR-REF: Non existent attrbute name: ~A in datastore descriptors ~A"
       name (descriptors ds)))

    ;; 1.5 If appending, then ensure we resize if need be and there is space.
    (when (eq index :end)
      ;; check to see if I need to resize, and if I can.
      (when (>= (appending-index attr-desc) (size ds))
        (if (resizeable-p ds)
            (resize ds)
            (error "SETF ATTR-REF: Attempting to resize a non-resizable datastore while appending to :end for attribute ~A" name)))

      ;; Then, convert the index into something meaningful
      (setf index (appending-index attr-desc)))

    ;; Ensure we can actually perform this write.
    (when (>= index (size ds))
      (error
       "SETF ATTR-REF: Out of bounds index ~A for datastore of size ~A~%"
       index (size ds)))

    ;; Now that we have an index, ensure the appending index is consistent
    ;; with it. This will not update when we write into attributes less than
    ;; the max we've ever written.
    (when (>= index (appending-index attr-desc))
      (setf (appending-index attr-desc) (1+ index)))

    ;; 2. Find the start byte of the attribute at the specified index.
    (let ((attr-start-byte-idx
           (+ (offset attr-desc)
              (* index (stride attr-desc)))))

      ;; 3. Analyze components, so we know what we're going to set.
      (cond
        ((null components)
         ;; A. No components specified, we're setting all components.
         (let* ((num-components (attr-count (attr attr-desc))))

           ;; B. Insert the components
           (vec->sv/unsigned-byte (attr-type (attr attr-desc))
                                  (data ds)
                                  attr-start-byte-idx
                                  comp-vec
                                  0
                                  (attr-count (attr attr-desc)))

           ;; Don't have a good return value here...
           (attr-count (attr attr-desc))))

        (components
         ;; B. There are specific components being referenced.
         ;; Store the components in the order specified into the result.
         (loop
            :for component :in components
            :for read-idx :by 1 :do
            (let* ((component-index
                    (if (numberp component)
                        component
                        (position component
                                  (attr-accessors (attr attr-desc)))))
                   (component-offset
                    (nth component-index
                         (attr-component-offsets (attr attr-desc)))))

              (vec->sv/unsigned-byte
               (attr-type (attr attr-desc))
               (data ds)
               (+ attr-start-byte-idx component-offset)
               comp-vec
               read-idx
               1)))

         ;; Don't have a good return value here.
         (length components))))))
