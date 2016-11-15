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

(defun %analyze-spec (spec)
  (flet ((has-location-p (attr)
           (member :location attr))
         (location-integerp (attr)
           (integerp (getf (cdr attr) :location)))
         (location-minusp (attr)
           (minusp (getf (cdr attr) :location)))
         (bail (analysis kind value)
           (return-from %analyze-spec
             (values analysis kind value))))
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
              :attribute-name-duplicated
              (select-if (lambda (attr)
                           (member (first attr) duplicate-attrs))
                         spec))))
    ;; Check attribute locations
    (unless (some #'has-location-p spec)
      (bail :undefined :attribute-locations-undefined spec))
    (when (/= (length spec) (count-if #'has-location-p spec))
      (bail :error
            :attribute-location-partially-defined
            (select-if (complement #'has-location-p) spec)))
    (unless (every #'location-integerp spec)
      (bail :error
            :attribute-location-type-error
            (select-if (complement #'location-integerp) spec)))
    (when (some #'location-minusp spec)
      (bail :error
            :attribute-location-range-error
            (select-if #'location-minusp spec)))
    (values :defined :attribute-locations-defined spec)))

(defun %assign-attribute-locations (attribute-set)
  (loop :for (name . properties) :in (spec attribute-set)
        :for attr = (gethash name (attributes attribute-set))
        :for location :from 0
        :do (setf (attr-location attr) location)))

(defun make-attribute-set (&rest spec)
  (let ((attribute-set (%make-attribute-set :spec (copy-seq spec))))
    (multiple-value-bind (result kind value)
        (%analyze-spec spec)
      (ecase result
        ((:undefined :defined)
         (loop :with attributes = (attributes attribute-set)
               :for (name . properties) :in spec
               :for attr = (apply #'%make-attribute :name name properties)
               :do (setf (gethash name attributes) attr))
         (when (eq result :undefined)
           (%assign-attribute-locations attribute-set)))
        (:error
         (gloss-error kind value))))
    attribute-set))
