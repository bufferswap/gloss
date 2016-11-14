(in-package :gloss.vao)

(defparameter *gloss-message-db*
  (let ((db (make-hash-table)))
    (mapcar
     (lambda (entry)
       (destructuring-bind (kind format-string) entry
         (setf (gethash kind db) format-string)))
     ;; Hrm, this representation allows spreading this stuff across files....
     '((attribute-name-duplicated
        "Attribute name is defined more than once.
These attributes have duplicate names:~%~(~{  ~S~%~}~)")
       (attribute-locations-undefined
        "All attributes in this set have no :LOCATION defined.
These attributes have no :LOCATION definitions:~%~(~{  ~S~%~}~)")
       (attribute-location-partially-defined
        "If any attribute has a :LOCATION, all other attributes must also.
These attributes need their locations defined:~%~(~{  ~S~%~}~)")
       (attribute-location-type-error
        "Attribute value for :LOCATION must be an integer.
These attributes do not have integer :LOCATION values:~%~(~{  ~S~%~}~)")
       (attribute-location-range-error
        "Attribute value :LOCATION must not be negative.
These attributes have negative :LOCATION values:~%~(~{  ~S~%~}~)")
       (attribute-locations-defined
        "All attributes in this set use :LOCATION properly.
These attributes use :LOCATION properly: ~(~{  ~S~%~}~)")))
    db))

(defun gloss-message-new (kind)
  (multiple-value-bind (message presentp) (gethash kind *gloss-message-db*)
    (if presentp
        message
        (format nil "Unknown gloss-message kind: ~A" kind))))



(defmethod gloss-message ((kind (eql 'attribute-name-duplicated)))
  "Attribute name is defined more than once.
These attributes have duplicate names:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql 'attribute-locations-undefined)))
  "All attributes in this set have no :LOCATION defined.
These attributes have no :LOCATION definitions:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql 'attribute-location-partially-defined)))
  "If any attributes has :LOCATION defined, all other attributes must also.
These attributes need their locations defined:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql 'attribute-location-type-error)))
  "Attribute value for :LOCATION must be an integer.
These attributes do not have integer :LOCATION values:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql 'attribute-location-range-error)))
  "Attribute value :LOCATION must not be negative.
These attributes have negative :LOCATION values:~%~(~{  ~S~%~}~)")

(defmethod gloss-message ((kind (eql 'attribute-locations-defined)))
  "All attributes in this set use :LOCATION properly.
These attributes use :LOCATION properly: ~(~{  ~S~%~}~)")
