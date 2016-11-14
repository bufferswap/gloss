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

(defun %analyze-attribute-locations (spec)
  (flet ((has-location-p (attr)
           (member :location attr))
         (location-integerp (attr)
           (integerp (getf (cdr attr) :location)))
         (location-minusp (attr)
           (minusp (getf (cdr attr) :location)))
         (bail (analysis kind context)
           (return-from %analyze-attribute-locations
             (values analysis kind context)))
         (select-if (filter lst)
           (mapcar #'identity (remove-if (complement filter) lst))))
    ;; Check for duplicate attribute names
    (let ((duplicate-attrs))
      (loop :with count = (make-hash-table)
            :for (name . nil) :in spec
            :if (nth-value 1 (gethash name count))
              :do (incf (gethash name count))
                  (push name duplicate-attrs)
            :else
              :do (setf (gethash name count) 1))
      (when duplicate-attrs
        (bail :error
              'attribute-name-duplicated
              (list (select-if (lambda (attr)
                                 (member (first attr) duplicate-attrs))
                               spec)))))
    ;; Check attribute locations
    (unless (some #'has-location-p spec)
      (bail :undefined
            'attribute-locations-undefined
            (list spec)))
    (when (/= (length spec) (count-if #'has-location-p spec))
      (bail :error
            'attribute-location-partially-defined
            (list (select-if (complement #'has-location-p) spec))))
    (unless (every #'location-integerp spec)
      (bail :error
            'attribute-location-type-error
            (list (select-if (complement #'location-integerp) spec))))
    (when (some #'location-minusp spec)
      (bail :error
            'attribute-location-range-error
            (list (select-if #'location-minusp spec))))
    (values :defined
            'attribute-locations-defined
            (list spec))))

(defun %assign-attribute-locations (attribute-set)
  (loop :for (name . properties) :in (spec attribute-set)
        :for attr = (gethash name (attributes attribute-set))
        :for location :from 0
        :do (setf (attr-location attr) location)))

(defun make-attribute-set (&rest spec)
  (let ((attribute-set (%make-attribute-set :spec (copy-seq spec))))
    (multiple-value-bind (analysis kind context)
        (%analyze-attribute-locations spec)
      (ecase analysis
        ((:undefined :defined)
         (loop :with attributes = (attributes attribute-set)
               :for (name . properties) :in spec
               :for attr = (apply #'%make-attribute :name name properties)
               :do (setf (gethash name attributes) attr))
         (when (eq analysis :undefined)
           (%assign-attribute-locations attribute-set)))
        (:error
         (gloss-error kind context))))
    attribute-set))

;;; Usage

#++(let ((attr-set (make-attribute-set
                    '(position :type :float :count 3 :accessors (px py pz))
                    '(normals :type :float :count 3 :accessors (nx ny nz))
                    '(uvs :type :float :count 3 :accessors (uvx uvy uvz)))))
     attr-set)
