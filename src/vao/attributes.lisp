(in-package :gloss.vao)

(defstruct (attribute-set (:constructor %make-attribute-set)
                          (:conc-name nil))
  spec
  (attributes (make-hash-table)))

(defstruct (attribute (:constructor %make-attribute)
                      (:conc-name attr-))
  (name nil)
  (type :float)
  (location nil)
  (count 1)
  (normalizep nil)
  (divisor 0)
  (accessors nil))

(defun %analyze-attribute-locations (attribute-set)
  (flet ((has-location-p (attr)
           (member :location attr))
         (location-integerp (attr)
           (integerp (getf (cdr attr) :location)))
         (location-minusp (attr)
           (minusp (getf (cdr attr) :location)))
         (bail (result reason)
           (return-from %analyze-attribute-locations
             (values result reason)))
         (spec-hide-attrs (spec fn)
           (mapcar #'first (remove-if fn spec))))
    (with-slots (spec) attribute-set
      (unless (find :location spec
                    :test (lambda (item attr) (member item attr)))
        (bail :undefined
              (list 'attribute-locations-undefined spec)))
      (when (/= (length spec) (count-if #'has-location-p spec))
        (bail :error
              (list 'attribute-locations-partially-defined
                    (spec-hide-attrs spec #'has-location-p))))
      (when (find-if (complement #'location-integerp) spec)
        (bail :error
              (list 'attribute-locations-type-error
                    (spec-hide-attrs spec #'location-integerp))))
      (when (find-if #'location-minusp spec)
        (bail :error
              (list 'attribute-locations-range-error
                    (spec-hide-attrs spec (complement #'location-minusp)))))
      (values :defined
              (list 'attribute-locations-defined spec)))))

(defun %assign-attribute-locations (attribute-set)
  (loop :for (name . properties) :in (spec attribute-set)
        :for attr = (gethash name (attributes attribute-set))
        :for location :from 0
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
    (multiple-value-bind (analysis reason)
        (%analyze-attribute-locations attribute-set)
      (destructuring-bind (kind value) reason
        (ecase analysis
          (:defined
           nil)
          (:undefined
           (%assign-attribute-locations attribute-set))
          (:error
           (gloss-error kind value)))))
    attribute-set))

;;; Usage

#++(let ((attr-set (make-attribute-set
                    '(position :type :float :count 3 :accessors (px py pz))
                    '(normals :type :float :count 3 :accessors (nx ny nz))
                    '(uvs :type :float :count 3 :accessors (uvx uvy uvz)))))
     attr-set)
