(in-package :gloss.vao)

;; TODO: Implement a post-process phase just when uploading the data
;; array to the GPU which does things like compact the :block
;; attributes to be continguous, etc, etc, etc. (and then uncompacts it).
;; This function is probably in the gl-buffer-data uploader for the datastore.



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
   ;; Keep track of the highest actual index which stores an attribute.
   ;; This implies a setf was performed (either by the user or with :end).
   (%max-defined-index :initarg :max-defined-index
                       :initform -1
                       :accessor max-defined-index)

   ;; The gl:get-attrib-location for this particular attribute.
   ;; This is by the user.
   (%raw-attrib-location :initarg :raw-attrib-location
                         :initform -1
                         :accessor raw-attrib-location)
   ;; TODO Add in if the uniform location should be enabled or not.

   ))

(defgeneric num-attrs (attr-desc)
  (:documentation
   "Return the number of attributes that are considered defined by this
ATTR-DESC. In practice this means one more than the highest index stored
into the datastore with which this ATTR-DESC is associated."))

(defmethod num-attrs ((attr-desc attribute-descriptor))
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
                  ;; The datastore can be changed to and from being resizeable,
                  ;; however, in doing so there may be serious performance
                  ;; consequences. The setter for resizeable-p must be hand
                  ;; written since a pile of work has to happen in that
                  ;; method.
                  :reader resizeable-p)
   ))

(defclass datastore-array-buffer (datastore) ())
(defclass datastore-element-array-buffer (datastore) ())
(defclass datastore-copy-buffer (datastore) ())
(defclass datastore-pixel-buffer (datastore) ())
(defclass datastore-query-buffer (datastore) ())
(defclass datastore-texture-buffer (datastore) ())
(defclass datastore-transform-feedback-buffer (datastore) ())
(defclass datastore-uniform-buffer (datastore) ())
(defclass datastore-draw-indirect-buffer (datastore) ())
(defclass datastore-atomic-counter-buffer (datastore) ())
(defclass datastore-dispatch-indirect-buffer (datastore) ())
(defclass datastore-shader-storage-buffer (datastore) ())

;; This derived type of datastore has methods on it to understand the
;; fact that this type represents binary level attribute layouts in
;; native machine memory arrays (such as static-vectors) which are
;; suitable for upload to the GPU.
(defclass native-datastore (datastore) ())

(defgeneric named-layout (ds)
  (:documentation "Get the named-layout datastore DS is referencing."))

(defmethod named-layout ((ds datastore))
  (lookup-named-layout (name ds) (layout-set ds)))

(defgeneric attr-set (ds)
  (:documentation "Get the attribute-set datastore DS is referenceing."))

(defmethod attr-set ((ds datastore))
  (attribute-set (layout-set ds)))



(defgeneric map-attr-descs (func ds)
  (:documentation "Map the FUNC across the attribute descriptors in
the order of the attribute template specification in datastore DS and
return the list."))

(defmethod map-attr-descs (func (ds datastore))
  (loop :for attr-name :in (template (named-layout ds))
     :for attr-desc = (gethash attr-name (descriptors ds))
     :collect (funcall func attr-desc)))

;; TODO: THis is really a stride value. Note its meaning for :block is odd.
(defgeneric attr-group-byte-size (ds)
  (:documentation "How big, in bytes is one complete grouping of the elements
describes in the template of the datastore. This includes the (possible)
alignment size of the elements of the attribute group if required."))

(defmethod attr-group-byte-size ((ds datastore))
  (reduce #'+ (map-attr-descs #'aligned-byte-length ds)))

;; TODO: Name is a bit funny.
(defgeneric attr-start-byte-offset (ds attr-name)
  (:documentation "Return the offset to the start of the attribute
data in this datastore. Return NIL of the attr-name is not valid for
this datastore."))

(defmethod attr-start-byte-offset (ds attr-name)
  (multiple-value-bind (desc present-p)
      (gethash attr-name (descriptors ds))
    (when present-p
      (offset desc))))

;; dealing with attrib locations.
(defmethod attrib-location (ds attr-name)
  (multiple-value-bind (desc present-p)
      (gethash attr-name (descriptors ds))
    (when present-p
      (attrib-location desc))))

(defmethod (setf attrib-location) (newval ds attr-name)
  (multiple-value-bind (desc present-p)
      (gethash attr-name (descriptors ds))
    (when present-p
      (setf (attrib-location desc) newval))))






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









;; TODO: This returns a datastore to store the data appropriate for
;; the definition of the datastore-name whose data type is
;; always unsigned-byte.
(defun make-native-datastore (datastore-name layout-set
                              &key (size 1024) (resizeable-p NIL))
  "Return a datastore which will store attribute data found in LAYOUT-SET
and identified by DATASTORE-NAME. The keyword arguments :SIZE defaults to 1024
and :RESIZEABLE-P defaults to NIL."
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
    (gen-data-storage datastore)

    datastore))





(defgeneric destroy-datastore (ds)
  (:documentation "Manually free any arrays (if appropriate) for
datastore DS."))

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







(defun compute-data-size-in-bytes (ds)
  "Compute the total size, in bytes, of a native data array
which will be used to store (possibly) aligned attribute data."
  (* (size ds) ;; The number of attribute groups....

     ;; Compute the size of one "group" (which is the total size of
     ;; all aligned attributes that can be referenced by the same
     ;; index in the datastore).

     ;; It turns out, that no matter how the attributes are actually layed out,
     ;; either :interleave, :block, or :seperate, that the total bytes devoted
     ;; to it are identical in each case at a given size.
     (attr-group-byte-size ds)))








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
                       :stride (attribute-size attr)
                       :aligned-byte-length (compute-attr-alignment
                                             attr
                                             named-layout))))

                  (template named-layout))))

    ;; 2. compute offsets for each attribute-descriptor
    (loop
       :with offset = 0
       :for desc :in descriptors :do
       ;; compute offset
       (setf (offset desc) offset)
       ;; and increment the offset to the start after SIZE amount of
       ;; the current attribute is stored.
       (incf offset (* (size ds)
                       (compute-attr-alignment (attr desc) named-layout))))

    ;; 3. Insert all descriptors into hash table
    (loop
       :for desc :in descriptors
       :for attr-name :in (template named-layout) :do
       (setf (gethash attr-name attribute-desc-table) desc))

    ;; Then, set the hash table into the datastore.
    (setf (descriptors ds) attribute-desc-table)))






(defun gen-data-storage (ds)
  "Determine what underlying type and how big to allocate the storage
for datastroe DS of KIND. Often the storage will be a static-vectors
type, but it can be other more complex things too, depending upon the
circumstances. Notice that no matter the actual underlying data
formate of :interleave, :block: or :seperate, this will do the right
thing."
  (let ((data-size-in-bytes (compute-data-size-in-bytes ds)))

    (format t "Allocating data :unsigned-byte size of: ~A~%"
            data-size-in-bytes)

    ;; 3. Fix up rest of datastore object.
    (setf (data-type ds) :unsigned-byte ;; the GL type...

          (data ds) (allocate-gl-typed-static-vector data-size-in-bytes
                                                     (data-type ds))))
  ds)






(defun construct-new-block-offsets (ds new-size)
  "This function is really only meaningful when the DS is a daatstore holding
a :block datastore. Return two values, the first is a list, in template order,
of the byte offsets of the beginning of each block attribute. The second
one is the same, but as if the block attributes should be layed out in the
native data store with NEW-SIZE amount of attributes. This function does not
alter any data in DS."
  (let ((old-offsets
         (loop :for name :in (template (named-layout ds))
            :collect (offset (gethash name (descriptors ds)))))

        (new-offsets
         (loop
            :with offset = 0
            :for name :in (template (named-layout ds))
            :for desc = (gethash name (descriptors ds))
            :collect offset :into offsets
            :do
            (incf offset
                  (* new-size
                     (compute-attr-alignment (attr desc)
                                             (named-layout ds))))
            :finally (return offsets))))

    (values old-offsets new-offsets)))




(defgeneric resize (ds)
  (:documentation "Resize a data store and ensure the new data is
properly maintained."))

(defmethod resize ((ds native-datastore))
  (let ((ds-kind (data-format (properties (named-layout ds)))))

    (format t "Resizing a ds of kind ~A~%" ds-kind)

    (let* (;; byte size of the descriptors in toto
           (attr-group-size (attr-group-byte-size ds))
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
        (ecase ds-kind
          ((:separate :interleave)
           ;; Copy the old data into the new data (as 'unsigned-byte).
           ;; Allocation fills the array already with zero data, so we
           ;; don't have to manage dealing with the unused places.
           (replace new-data (data ds)))
          (:block
           ;; Here, we must recompute the attribute descs and then
           ;; carefully move the data from the old array at the old
           ;; offsets to the new array at the new offsets.

           ;; 1. in template order, get the current offsets for the
           ;; current size
           ;;
           ;; 2. in template order, compute the new offsets for the
           ;; new size
           (multiple-value-bind (old-offsets new-offsets)
               (construct-new-block-offsets ds new-size)

             (format t "new size in attr groups: ~A~%" new-size)
             (format t "old offsets: ~A~%" old-offsets)
             (format t "new offsets: ~A~%" new-offsets)

             ;; 3. for each template attribute, REPLACE the strip of data
             ;; it has from the old array into the new array at the right
             ;; offset for the expanded size.
             (loop
                :for name :in (template (named-layout ds))
                :for old-off :in old-offsets
                :for new-off :in new-offsets
                :for desc = (gethash name (descriptors ds))
                ;; The total number of possibly defined bytes
                ;; devoted strictly and contiguously to NAME's
                ;; data.
                :for total-attr-bytes = (* (size ds)
                                           (compute-attr-alignment
                                            (attr desc)
                                            (named-layout ds)))
                :do
                (format
                 t "Copy ~A from byte index range ~A:~A to byte index ~A~%"
                 name old-off (+ old-off total-attr-bytes)
                 new-off)

                (replace new-data (data ds)
                         :start1 new-off
                         :start2 old-off :end2 (+ old-off total-attr-bytes)))

             ;; 4. Store the new offsets into the attr descs, I can't
             ;; really call GEN-ATTRIBUTE-DESCRIPTORS again when the
             ;; size is changed cause I need all the other data in
             ;; there to be left alone.
             ;;
             ;; TODO: Meh, this is kinda nasty to just jam them in here.
             (loop :for name :in (template (named-layout ds))
                :for new-off :in new-offsets :do
                (setf (offset (gethash name (descriptors ds))) new-off))

             )))

        ;; free the native array!
        (static-vectors:free-static-vector (data ds))

        ;; reset the object to the new stuff.
        (setf (data ds) new-data
              (size ds) new-size))

      ds)))





(defgeneric attr-ref (ds name index &rest components)
  (:documentation "Return the data for attribute NAME at INDEX in the datastore DS. If COMPONENTS is supplied, then produce a swizzle as desired. Always
returns a newly allocated vector."))

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


(defgeneric (setf attr-ref) (comp-vec ds name index &rest components)
  (:documentation "DOCUMENT ME."))

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
                                  num-components)

           ;; And record the potential new highest
           (setf (max-defined-index attr-desc)
                 (max (max-defined-index attr-desc) index))

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

         ;; And record the potential new highest
         (setf (max-defined-index attr-desc)
               (max (max-defined-index attr-desc) index))

         ;; Don't have a good return value here.
         (length components))))))







(defgeneric consistent-attributes-p (ds)
  (:documentation
   "Return T if the attribute data is consistent and NIL otherwise.
TODO: Describe consistency (basically it means all attribute groups up to
the max index set are defined)."))

(defmethod consistent-attributes-p ((ds native-datastore))
  (let ((max-indices-list
         (loop :for desc :being :the :hash-values :in (descriptors ds)
            :collect (max-defined-index desc))))
    (and (apply #'= max-indices-list)
         ;; TODO: I'm unsure about this test. If you have no
         ;; attributes in a datastore, is it consistent? Currently, I
         ;; say no, but that might change in the future.
         (every #'identity (mapcar (lambda (v) (/= v -1))
                                   max-indices-list)))))





;; XXX This code is correct, but in the wrong context.
(defun coalesce-data (ds)
  "This function shrinks the attribute data to be contiguus. It is
expected that after this call, the data is uploaded to the GPU and then
uncoalesce-data is called right afterwards."
  (let ((data-format (data-format (properties (named-layout ds)))))

    ;; Check to see if we should both doing this.
    (unless (eq data-format :block)
      (return-from coalesce-data NIL))

    ;; This number is the number of the defined attributes
    ;; as according to the user who was inserting them. If this is
    ;; a :block for example, then this is the most of one of the
    ;; attributes. All others are considered defined up to this number,
    ;; but if they aren't actually defined, they get zero.
    (let ((num-defined-attrs
           (apply #'max (map-attr-descs #'num-attrs ds))))

      ;; Create the new offsets which lay the data out contiguously.
      (multiple-value-bind (old-offsets new-offsets)
          (construct-new-block-offsets ds num-defined-attrs)

        ;; I should fix this to probably ignore the first replacement.
        ;; We specfically go from left to right in the template to processes
        ;; this so we don't overwrite anything bad.
        (loop
           :for name :in (template (named-layout ds))
           :for old-off :in old-offsets
           :for new-off :in new-offsets
           :for desc = (gethash name (descriptors ds))
           ;; The total number of possibly defined bytes
           ;; devoted strictly and contiguously to NAME's
           ;; data.
           :for total-attr-bytes = (* num-defined-attrs
                                      (compute-attr-alignment
                                       (attr desc)
                                       (named-layout ds)))
           :do
           (format
            t "Coalesce ~A from byte index range ~A:~A to byte index ~A~%"
            name old-off (+ old-off total-attr-bytes)
            new-off)

           (replace (data ds)
                    (data ds)
                    :start1 new-off
                    :start2 old-off :end2 (+ old-off total-attr-bytes))

           ;; Then, zero out the space between the end of the new and the old.
           ;; Don't need this since it'll be overwritten by later copies.
           ;; This catches cases where there are an uneven number of attributes
           ;; for each attribute and doesn't leave incorrect data in the array.
           (loop :for index
              :from (+ new-off total-attr-bytes)
              :below (+ old-off total-attr-bytes)
              :do (setf (aref (data ds) index) 0))
           )


        ;; At the end of this copying, we wipe out the remainder bytes to
        ;; be zero.
        (let ((end-of-coalesced-data-index
               (* num-defined-attrs
                  (attr-group-byte-size ds))))

          (loop
             :for index :from end-of-coalesced-data-index
             :below (length (data ds))
             :do (setf (aref (data ds) index) 0)))

        ;; Yes, I did work, return something which allows me to undo it
        (values num-defined-attrs old-offsets new-offsets)))))



(defun uncoalesce-data (ds num-defined-attrs orig-offsets
                        coalesced-offsets)
  "Assuming the DS is in coalesced state, uncoalesce the data back into the
original size of the DS. It is assumed that the size of the DS will be
greater than or equal to the NUM-DEFINED-ATTRS. The ORIG-OFFSETS was the
original location of the data and the coalesced offsets are the current
locations of the coalesced data."
  (let ((data-format (data-format (properties (named-layout ds)))))

    ;; Check to see if we should both doing this.
    (unless (eq data-format :block)
      (return-from uncoalesce-data NIL))

    ;; Create the new offsets which restore the data to the original
    ;; position
    (let ((old-offsets coalesced-offsets)
          (new-offsets orig-offsets))

      ;; We specfically go from right to left in the template to processes
      ;; this so we don't overwrite anything bad.
      (loop
         :for name :in (reverse (template (named-layout ds)))
         :for old-off :in (reverse old-offsets)
         :for new-off :in (reverse new-offsets)
         :for desc = (gethash name (descriptors ds))
         ;; The total number of possibly defined bytes
         ;; devoted strictly and contiguously to NAME's
         ;; data.
         :for total-attr-bytes = (* num-defined-attrs
                                    (compute-attr-alignment
                                     (attr desc)
                                     (named-layout ds)))
         :do

         (format
          t "Uncoalesce ~A from byte index range ~A:~A to byte index ~A~%"
          name old-off (+ old-off total-attr-bytes)
          new-off)

         (replace (data ds)
                  (data ds)
                  :start1 new-off
                  :start2 old-off :end2 (+ old-off total-attr-bytes))

         ;; Then, zero out the space between the old start of the
         ;; attribute and the new start. This removes any extranoues
         ;; data left over.
         (loop :for index :from old-off :below new-off
            :do (setf (aref (data ds) index) 0)))

      T)))


(defgeneric commit-to-gpu (ds &key &allow-other-keys)
  (:documentation "Upload the datastore to the GPU using glBufferData()."))





;; TODO: In the :block format, things like the offset of the start of the
;; attributes are effectively with respect to the "coalesced" format of
;; the attribute data. So, in the interface, there is a discrepancy between
;; start offsets of attributes and whatnot between coalesced and non-colaesced
;; depending upon your viewpoint of the data.
;;
;; SO, if I don't allow the user access to the interesting

(defmethod commit-to-gpu ((ds native-datastore) &key (ensure-consistency-p T))
  ;; -1. This function assumes any external opengl state is set up when
  ;; it does the buffer data bit. Meaning, the vbo (or whatever other)
  ;; bindings must be bound before uploading the data.

  ;; 0. Ensure the attribute data in the descriptors is all consistent
  ;; (which functionally means the appending index for all of them must be the
  ;; same, representing that all attributes have been filled). Check
  ;; ensure-consistency-p to determine if I should signal a condition on not
  ;; consistent, or just assume zeros for missing data.

  ;; 1. Prepare the buffer for upload. In the case of :interleave and :separate
  ;; this is a nop. In the case of :block, it means compact the data to be
  ;; continguous in the native array.

  ;;(format t "Before coaclescing: ~A~%" (data ds))
  (multiple-value-bind (coalesced-defined-index orig-offsets coalesced-offsets)
      (coalesce-data ds)

    ;;(format t "After coalescing: ~A~%" (data ds))

    ;; 2. Upload the data. glBufferData(). But, only upload as much as
    ;; is actually used.
    (let ((max-defined-attrs (apply #'max (map-attr-descs #'num-attrs ds))))
      (%gl:buffer-data
       ;; Define what type of data store this should be, :array-buffer, etc.
       (binding-target (properties (named-layout ds)))
       ;; How many bytes am I uploading?
       (* max-defined-attrs (attr-group-byte-size ds))
       ;; pointer to the actual data
       (static-vectors:static-vector-pointer (data ds))
       ;; And the usage hint, :static-draw, etc
       (usage-hint (properties (named-layout ds))))

      ;; 3. Post process the buffer after upload. In the case of
      ;; :interleave and :separate, this is a nop. In the case of
      ;; :block, it means spread the data back out to match what is was
      ;; when this function was originally called.  Well, without
      ;; thinking too much, I believe it'll undo the attributes back to
      ;; how they used to be, but it might optiize the space left if
      ;; possible.
      (uncoalesce-data
       ds coalesced-defined-index orig-offsets coalesced-offsets)

      ;; 4. Return the number of attribute groups that have been
      ;; uploaded.
      ;;(format t "After uncoalescing: ~A~%" (data ds))

      max-defined-attrs)))
