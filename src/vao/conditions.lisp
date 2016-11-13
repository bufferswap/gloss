(in-package :gloss.vao)

;; While most of these represent some sort of error condition, some are
;; warnings, or other informative messages.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages about individual attributes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod gloss-message ((kind (eql 'attribute/name-duplicated)))
  "Attribute name is defined more than once: ~A")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages about attribute sets.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod gloss-message ((kind
                           (eql 'attribute-set/locations-not-defined)))
  "All attributes in this set have no :location defined: ~(~S~)")

(defmethod gloss-message ((kind
                           (eql 'attribute-set/locations-partially-defined)))
  "If some attributes use :location, all must use them. Here is a list of attributes that need :location defined in them: ~(~S~)")

(defmethod gloss-message ((kind
                           (eql 'attribute-set/location-type-error)))
  "Attribute :location values must be integers. Here is a list of attributes whose :location values are not integers: ~(~S~)")

(defmethod gloss-message ((kind
                           (eql 'attribute-set/location-range-error)))
  "Attribute :location values must be integers greater than or equal to zero. Here is a list of attributes whose :location values are negative: ~(~S~)")

(defmethod gloss-message ((kind
                           (eql 'attribute-set/locations-defined)))
  "All attributes in this set have :location properly defined: ~(~S~)")
