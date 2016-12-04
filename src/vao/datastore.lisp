(in-package :gloss.vao)

;; In a datastore, we record, for each attribute participating,
;; information about how that attribute is exactly layed out into the
;; datastore, how many entries of that attribute there are, etc, etc, etc.
(defstruct attribute-byte-layout
  ;; How long is the raw representation of the attribute.
  raw-byte-length
  ;; To what number should we align the attribute?
  alignment
  ;; If we align the attribute, how many bytes is it including the
  ;; wasted alignment bytes?
  aligned-byte-length
  ;; In the datastore, what is the byte offset of the first attribute in the
  ;; byte store?
  offset
  ;; What is the stride to the next chunk of this attributes data?
  stride
  ;; How many valid entries have we stored in the data store?
  num-valid-attributes
  ;; When we need to write the next attribute intot he data store, what is
  ;; the byte index at which we need to write it?
  byte-write-index)

(defstruct datastore-buffer
  ;; The actual static-vector storage for the attribute data.
  native-data
  ;; What fundamental native type is this array?
  native-type
  ;; A hash table of attributes keyed by attribute shortname
  ;; whose value is an attribute-info structure.
  attr-layout)
