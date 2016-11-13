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
         (select-if (filter lst &key (transform #'identity))
           (mapcar transform (remove-if (complement filter) lst))))

    ;; Check for duplicate attribute names.
    (let ((duplicate-count (make-hash-table)))
      (loop :for attr-name :in (mapcar #'first spec) :do
         (if (null (nth-value 1 (gethash attr-name duplicate-count)))
             (setf (gethash attr-name duplicate-count) 1)
             (incf (gethash attr-name duplicate-count))))
      (let ((duplicates ()))
        (maphash (lambda (name use-count)
                   (when (> use-count 1)
                     (push name duplicates)))
                 duplicate-count)
        (when duplicates
          (bail :error
                'attribute-name-duplicated
                (list (select-if (lambda (attr)
                                   (when (member (first attr) duplicates)
                                     attr))
                                 spec))))))

    ;; Check that no :locations are defined.
    (unless (find :location spec
                  :test (lambda (item attr) (member item attr)))
      (bail :undefined
            'attribute-locations-undefined
            (list spec)))
    ;; Check that all :locations are defined
    (when (/= (length spec) (count-if #'has-location-p spec))
      (bail :error
            'attribute-location-partially-defined
            (list (select-if (complement #'has-location-p) spec))))
    ;; Check that all :locations are integers.
    (when (find-if (complement #'location-integerp) spec)
      (bail :error
            'attribute-location-type-error
            (list (select-if (complement #'location-integerp) spec))))
    ;; Check that all :locations are not negative.
    (when (find-if #'location-minusp spec)
      (bail :error
            'attribute-location-range-error
            (list (select-if #'location-minusp spec))))
    ;; If all the above passes, then :locations are defined.
    (values :defined
            'attribute-locations-defined
            (list spec))))

(defun %assign-attribute-locations (attribute-set)
  (loop :for (name . properties) :in (spec attribute-set)
     :for attr = (gethash name (attributes attribute-set))
     :for location :from 0
     :do (setf (attr-location attr) location
               (getf properties :location) location)))

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
         (when :undefined
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
