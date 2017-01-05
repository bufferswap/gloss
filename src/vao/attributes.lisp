(in-package :gloss.vao)

(defstruct (attribute-set (:constructor %make-attribute-set)
                          (:conc-name nil))
  (spec nil)
  (attributes (make-hash-table)))

(defstruct (attribute (:constructor %make-attribute)
                      (:conc-name attr-))
  (name nil)
  (type :float)
  (location nil)
  (count 1)
  (normalizep nil)
  (divisor 0)
  (accessors nil)
  (component-offsets nil))

(defun attribute-type-size (attribute)
  "The size in bytes of an attribute's type."
  (ecase (attr-type attribute)
    ((:byte :unsigned-byte) 1)
    ((:short :unsigned-short :half-float) 2)
    ((:int :unsigned-int :float) 4)))

(defun attribute-size (attribute)
  "The size in bytes of an attribute when stored in a datastore."
  (* (attr-count attribute) (attribute-type-size attribute)))

(defun make-attribute (name properties)
  "Create an attribute object of the specified name and properties."
  (let* ((attr (apply #'%make-attribute :name name properties))
         (type-size (attribute-type-size attr)))
    (with-slots (count component-offsets) attr
      (setf component-offsets (iota count :step type-size)))
    attr))

(defun analyze-spec (spec)
  "Analyze the attributes specification for problems and return the findings, in order to be later
handled appropriately."
  (flet ((has-location-p (attr)
           (member :location attr))
         (location-integerp (attr)
           (integerp (getf (cdr attr) :location)))
         (location-minusp (attr)
           (minusp (getf (cdr attr) :location)))
         (bail (analysis kind value)
           (return-from analyze-spec
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
              (select-if (lambda (attr) (member (first attr) duplicate-attrs)) spec))))
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

(defun assign-attribute-locations (attribute-set)
  "Serially assign attribute locations to each attribute.
This is performed only if the attribute specification analysis detects that all attributes have no
location defined."
  (loop :for (name . properties) :in (spec attribute-set)
        :for attr = (gethash name (attributes attribute-set))
        :for location :from 0
        :do (setf (attr-location attr) location)))

(defun make-attribute-set (&rest spec)
  (let ((attribute-set (%make-attribute-set :spec (copy-seq spec))))
    (multiple-value-bind (result kind value)
        (analyze-spec spec)
      (ecase result
        ((:undefined :defined)
         (loop :with attributes = (attributes attribute-set)
               :for (name . properties) :in spec
               :for attr = (make-attribute name properties)
               :do (setf (gethash name attributes) attr))
         (when (eq result :undefined)
           (assign-attribute-locations attribute-set)))
        (:error
         (gloss-error kind value))))
    attribute-set))

(defun lookup-attribute (attribute-name attribute-set)
  (gethash attribute-name (attributes attribute-set)))
