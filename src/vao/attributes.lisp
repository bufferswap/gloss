(in-package :gloss.vao)

;;; Attributes

(defstruct (attribute (:constructor %make-attribute)
                      (:conc-name attr-))
  (name nil)
  (type :float)
  (location nil)
  (count 1)
  (normalizep nil)
  (divisor 0)
  (accessors nil))

(defun %attribute-location-valid-p (attribute)
  (let ((location (getf (cdr attribute) :location)))
    (identity location)))

(defun %ensure-attribute-locations (attribute-set)
  (with-slots (spec) attribute-set
    (let ((count (count-if #'%attribute-location-valid-p spec)))
      (cond
        ((= count 0)
         (%assign-attribute-locations attribute-set))
        ((not (= count (length spec)))
         ;; TODO: Make this a real error message instead of the default
         ;; undefined error.
         (gloss-error nil))))))

;;; Attribute Sets

(defstruct (attribute-set (:constructor %make-attribute-set)
                          (:conc-name nil))
  spec
  (attributes (make-hash-table)))

(defun %assign-attribute-locations (attribute-set)
  (loop :for (name . properties) :in (spec attribute-set)
        :for attr = (gethash name (attributes attribute-set))
        :for location = 0 :then (incf location)
        :do (setf (attr-location attr) location
                  (getf properties :location) location)))

(defun make-attribute-set (&rest spec)
  (let ((attribute-set (%make-attribute-set :spec (copy-seq spec))))
    (loop :with attributes = (attributes attribute-set)
          :for (name . properties) :in spec
          :for attr = (apply #'%make-attribute :name name properties)
          :when (nth-value 1 (gethash name attributes))
            :do (gloss-error 'attribute-name-duplicated name)
          :do (setf (gethash name attributes) attr))
    (%ensure-attribute-locations attribute-set)
    attribute-set))

;;; Usage

#++(let ((attr-set (make-attribute-set
                 '(position :type :float :count 3 :accessors (px py pz))
                 '(normals :type :float :count 3 :accessors (nx ny nz))
                 '(uvs :type :float :count 3 :accessors (uvx uvy uvz)))))
  attr-set)
