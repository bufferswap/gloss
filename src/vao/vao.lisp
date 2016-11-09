(in-package :gloss.vao)

;;; VAO Attributes

(defstruct (attribute (:constructor %make-attribute))
  (type :float)
  (location nil)
  (count 1)
  (normalizep nil)
  (divisor 0)
  (accessors nil))

(defun make-attribute (attribute-set attribute-name &rest attribute-data)
  (if (nth-value 1 (gethash attribute-name attribute-set))
      (error "Attribute ~A is defined more than once." attribute-name)
      (apply #'%make-attribute attribute-data)))

(defun add-attributes (attribute-set &rest attribute-specs)
  (loop :for (name . data) :in attribute-specs
        :for attr = (apply #'make-attribute attribute-set name data)
        :do (setf (gethash name attribute-set) attr)))

(defun assign-attribute-locations (attribute-specs)
  (loop :for attr :in attribute-specs
        :for location = 0 :then (incf location)
        :do (setf (getf (cdr attr) :location) location)))

(defun attribute-location-valid-p (attribute)
  (let ((location (getf (cdr attribute) :location)))
    (and (integerp location)
         (not (minusp location)))))

(defun ensure-attribute-locations (attribute-specs)
  (let ((count (count-if #'attribute-location-valid-p attribute-specs)))
    (cond
      ((= count 0)
       (assign-attribute-locations attribute-specs))
      ((not (= count (length attribute-specs)))
       (error "All attributes must have a non-negative :LOCATION defined.")))))

(defun make-attribute-set (&rest attribute-specs)
  (let ((attribute-set (make-hash-table)))
    (ensure-attribute-locations attribute-specs)
    (apply #'add-attributes attribute-set attribute-specs)
    attribute-set))

;;; Usage

#++(let ((attr-set (make-attribute-set
                 '(position :type :float :count 3 :accessors (px py pz))
                 '(normals :type :float :count 3 :accessors (nx ny nz))
                 '(uvs :type :float :count 3 :accessors (uvx uvy uvz)))))
  (add-attributes attr-set
                  '(some-attr-1 :type :byte :count 2)
                  '(some-attr-2 :type :unsigned-byte :count 2))
  attr-set)
