(in-package :gloss.vao)

(defmethod gloss-message ((condition (eql 'attribute-name-duplicated)))
  "Attribute name is defined more than once.")

(defmethod gloss-message ((condition (eql 'attribute-location-negative)))
  "Attribute has a negative :LOCATION.")
