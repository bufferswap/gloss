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

(defun %ensure-attribute-locations (attribute-set)
  (with-slots (spec attributes) attribute-set
    (loop :for attr :in spec
       :for serialnum :from 0 :do
       (setf (attr-location (gethash (first attr) attributes)) serialnum)))
  attribute-set)

(defun %analyze-location-usage (attribute-set)
  (flet ((has-location-p (attr)
           (member :location attr))
         (location-integerp (attr)
           (integerp (getf (cdr attr) :location)))
         (location-gteq-zero (attr)
           (>= (getf (cdr attr) :location) 0))
         (bail (result reason)
           (return-from %analyze-location-usage
             (values result reason))))

    (with-slots (spec) attribute-set
      ;; If none of the attributes use :location, we're done.
      (unless (find :location spec
                    :test (lambda (item attr) (member item attr)))
        (bail :none-defined
              (list 'attribute-set/locations-not-defined
                    spec)))

      ;; If we pass the above, then check to see that all :location
      ;; keywords are present.
      (when (/= (length spec) (count-if #'has-location-p spec))
        (bail :partially-defined
              (list 'attribute-set/locations-partially-defined
                    (remove-if #'has-location-p spec))))

      ;; If we pass the above, then ensure they are all actually integers.
      (when (find-if (complement #'location-integerp) spec)
        (bail :type-error
              (list 'attribute-set/location-type-error
                    (remove-if #'location-integerp spec))))

      ;; If we pass the above, then ensure they are positive
      (when (find-if (complement #'location-gteq-zero) spec)
        (bail :range-error
              (list 'attribute-set/location-range-error
                    (remove-if #'location-gteq-zero spec))))

      ;; If the above passes, we're done, all attributes have :locations and
      ;; they are well-defined.
      (values :defined
              (list 'attribute-set/locations-defined
                    spec)))))



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

    (multiple-value-bind (analysis reasons)
        (%analyze-location-usage attribute-set)
      (destructuring-bind (reason attr-name-set) reasons
        (format t "Analysis results:~%  analysis: ~S~%  reasons: ~S~%"
                analysis reasons)
        (ecase analysis
          (:defined
           ;; nothing to do, user did everything necessary.
           nil)
          (:none-defined
           ;; We must assign them automatically.
           (%ensure-attribute-locations attribute-set))
          ((:partially-defined :type-error :range-error)
           ;; These are errors
           (gloss-error reason attr-name-set)))))

    attribute-set))

;;; Usage

#++(let ((attr-set (make-attribute-set
                    '(position :type :float :count 3 :accessors (px py pz))
                    '(normals :type :float :count 3 :accessors (nx ny nz))
                    '(uvs :type :float :count 3 :accessors (uvx uvy uvz)))))
     attr-set)

;; These aren't really tests since they don't check any values.
(defun test-mls/pass/no-locations ()
  (make-attribute-set
   '(position :type :float :count 3)
   '(normal :type :float :count 3)
   '(uv :type :float :count 2)))

(defun test-mls/pass/all-good-locations ()
  (make-attribute-set
   '(position :type :float :count 3 :location 0)
   '(normal :type :float :count 3 :location 1)
   '(uv :type :float :count 2 :location 2)))

(defun test-mls/fail/partial-locations ()
  (make-attribute-set
   '(position :type :float :count 3)
   '(normal :type :float :count 3 :location 0)
   '(uv :type :float :count 3)))

(defun test-mls/fail/bad-type-location ()
  (make-attribute-set
   '(position :type :float :count 3 :location 0)
   '(normal :type :float :count 3 :location 1)
   '(uv :type :float :count 3 :location 'foo)))

(defun test-mls/fail/bad-range-location ()
  (make-attribute-set
   '(position :type :float :count 3 :location 0)
   '(normal :type :float :count 3 :location 1)
   '(uv :type :float :count 3 :location -5)))

(defun test-mls ()
  (test-mls/pass/no-locations)
  (test-mls/pass/all-good-locations)
  (test-mls/fail/partial-locations)
  (test-mls/fail/bad-type-location)
  (test-mls/fail/bad-range-location))
