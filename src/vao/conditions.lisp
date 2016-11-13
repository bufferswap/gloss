(in-package :gloss.vao)

(defmethod gloss-message ((kind (eql 'attribute-name-duplicated)))
  "Attribute name is defined more than once.")

(defmethod gloss-message ((kind (eql 'attribute-locations-undefined)))
  "All attributes in this set have no :LOCATION defined")

(defmethod gloss-message ((kind (eql 'attribute-locations-partially-defined)))
  "If any attributes has :LOCATION defined, other attributes must.
Attributes that need locations defined")

(defmethod gloss-message ((kind (eql 'attribute-locations-type-error)))
  "Attribute :LOCATION must be an integer.
Attributes with locations that are not an integers")

(defmethod gloss-message ((kind (eql 'attribute-locations-range-error)))
  "Attribute :LOCATION must not be negative.
Attributes with locations that are negative")

(defmethod gloss-message ((kind (eql 'attribute-locations-defined)))
  "All attributes in this set have :LOCATION properly defined")
