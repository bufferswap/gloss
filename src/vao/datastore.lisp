(in-package :gloss.vao)

;; In a datastore, we record, for each attribute participating,
;; information about how that attribute is exactly layed out into the
;; datastore, how many entries of that attribute there are, etc, etc, etc.
(defstruct attribute-descriptor
  ;; Is this attribute aligned in the datastore?
  alignedp
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
;; We define a base class and later refine it to make it easier to
;; break apart how to deal with alignment of the attributes into the native
;; array.
(defclass datastore ()
  ;; To what should we align the start of each attribute?
  ;; This is the same for all attributes.
  ((%alignment :initarg :alignment
               :initform 1
               :accessor alignment)
   ;; The actual static-vector storage for the attribute data.
   (%native-data :initarg :native-data
                 :initform NIL
                 :accessor native-data)
   ;; A hash table keyed by attribute shortname and whose value is
   ;; an attribute-descriptor structure.
   (%attr-layout :initarg :attr-layout
                 :initform (make-hash-table)
                 :accessor attr-layout)))



;; These represent the element size of the underlying native-data
;; array. When we make a datastore, we pick the best one to in
;; relation to the component data types in all attributes being stored
;; in the datastore or based upon alignment requirements.
(defclass datastore-unsigned-byte (datastore) ())
(defclass datastore-unsigned-short (datastore) ())
(defclass datastore-unsigned-int (datastore) ())

(defun make-datastore (datastore-name layout-set)
  ;; TODO: Implement me.
  nil)
