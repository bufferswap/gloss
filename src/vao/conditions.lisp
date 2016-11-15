(in-package :gloss.vao)

(defmethod gloss-message ((kind (eql :attribute-name-duplicated)))
  "Attribute name is defined more than once.
These attributes have duplicate names:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql :attribute-locations-undefined)))
  "All attributes in this set have no :LOCATION defined.
These attributes have no :LOCATION definitions:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql :attribute-location-partially-defined)))
  "If any attributes has :LOCATION defined, all other attributes must also.
These attributes need their locations defined:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql :attribute-location-type-error)))
  "Attribute value for :LOCATION must be an integer.
These attributes do not have integer :LOCATION values:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql :attribute-location-range-error)))
  "Attribute value :LOCATION must not be negative.
These attributes have negative :LOCATION values:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql :attribute-locations-defined)))
  "All attributes in this set use :LOCATION properly.
These attributes use :LOCATION properly: ~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql :attribute-undefined)))
  "Attribute is not defined: ~S")
