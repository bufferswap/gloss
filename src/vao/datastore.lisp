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


(defun attr/gl-float->static-vector/unsigned-byte
    (out-svec write-byte-index in-vec read-element-index)
  "Read a complete attribute whose component type is intended to be
the GL type :float from (the CL vector/array) in-vec at
read-element-index (one index per component) then convert each
component to a byte representation and write it into the static-vector
out-svec at the write-byte-index location.  Return the values of
out-svec and the number of bytes written."

  (values out-svec 0))

(defun attr/gl-float->static-vector/unsigned-int
    (out-svec write-byte-index in-vec read-element-index)
  "Read a complete attribute whose component type is intended to be
the GL type :float from (the CL vector/array) in-vec at
read-element-index (one index per component) then convert each
component to a byte representation and write it into the static-vector
out-svec at the write-byte-index location.  Return the values of
out-svec and the number of bytes written."

  (values out-svec 0))
