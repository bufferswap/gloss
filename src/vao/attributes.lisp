(in-package :gloss.vao)

;;; Attributes

(defstruct (attribute (:constructor %make-attribute))
  (type :float)
  (location nil)
  (count 1)
  (normalizep nil)
  (divisor 0)
  (accessors nil))

(defun make-attribute (attribute-set attribute-name &rest attribute-data)
  (if (nth-value 1 (gethash attribute-name attribute-set))
      (gloss-error "Attribute is defined more than once."
                   :value (gethash attribute-name attribute-set))
      (apply #'%make-attribute attribute-data)))

(defun %attribute-location-valid-p (attribute)
  (let ((location (getf (cdr attribute) :location)))
    (and (integerp location)
         (not (minusp location)))))

(defun %assign-attribute-locations (attribute-specs)
  (loop :for attr :in attribute-specs
        :for location = 0 :then (incf location)
        :do (setf (getf (cdr attr) :location) location)))

(defun %ensure-attribute-locations (attribute-specs)
  (let ((count (count-if #'%attribute-location-valid-p attribute-specs)))
    (cond
      ((= count 0)
       (%assign-attribute-locations attribute-specs))
      ((not (= count (length attribute-specs)))
       (gloss-error "All attributes must have a non-negative :LOCATION defined."
                    :value attribute-specs)))))

;;; Attribute Sets

(defstruct (attribute-set (:constructor %make-attribute-set)
                          (:conc-name nil))
  (attributes (make-hash-table)))

(defun make-attribute-set (&rest attribute-specs)
  (let ((attribute-set (%make-attribute-set)))
    (%ensure-attribute-locations attribute-specs)
    (loop :with attributes = (attributes attribute-set)
          :for (name . data) :in attribute-specs
          :for attr = (apply #'make-attribute attributes name data)
          :do (setf (gethash name attributes) attr))
    attribute-set))

;;; Usage

#++(let ((attr-set (make-attribute-set
                 '(position :type :float :count 3 :accessors (px py pz))
                 '(normals :type :float :count 3 :accessors (nx ny nz))
                 '(uvs :type :float :count 3 :accessors (uvx uvy uvz)))))
  attr-set)
