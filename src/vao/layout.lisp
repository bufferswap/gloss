(in-package :gloss.vao)

;;; VAO Layouts

(defstruct (datastore-properties (:constructor %make-datastore-properties)
                                 (:conc-name nil))
  data-format
  (binding-target :array-buffer)
  (usage-hint :static-draw))

(defstruct (datastore-layout-set (:constructor %make-datastore-layout-set)
                                 (:conc-name nil))
  attr-set
  primitive-kind
  (datastore-layouts (make-hash-table))
  (attribute-view (make-hash-table)))

(defstruct (datastore-layout (:constructor %make-datastore-layout)
                             (:conc-name nil))
  properties
  template)

;; This does little error checking, it shoud do more.
(defun make-layout-set (attr-set primitive-kind &rest datastore-specs)
  (let (;; The hash table holding the datastore layout information
        (hash/name->layout (make-hash-table :test #'eq))
        ;; The hasn table holding the attribute-view
        (hash/attr-view (make-hash-table :test #'equal))
        ;; We keep track of how many times we used each attribute in all
        ;; datastore-specs
        (hash/attr-num-use (make-hash-table :test #'eq)))

    ;; Loop over each datastore-specification
    (loop :for (properties . named-layouts) :in datastore-specs :do
       ;; For each name-layout, we'll add an entry to a hash table.
       (loop :for (name template) :in named-layouts :do
          ;; insert an entry.
          (setf (gethash name hash/name->layout)
                (%make-datastore-layout
                 :properties
                 ;; We denormalize the single properties information into
                 ;; individual copies of the inforamtion for each datastore
                 ;; description.
                 (apply #'%make-datastore-properties
                        (reduce #'append properties :initial-value NIL))
                 :template template))

          ;; Walk the template and add a formal name to the attr-view
          (loop :for attr-name :in template :do
             (let ((formal-name (list name attr-name)))
               ;; validate that it is in the pased in attr-set
               (unless (gethash attr-name (attributes attr-set))
                 (error "make-layout-set: attr-name ~A in not in the attr-set."
                        attr-name))

               ;; assign the formal name to the datastore name
               (setf (gethash formal-name hash/attr-view) name)

               ;; Count the uses of this attribute name
               (if (null (nth-value 1 (gethash attr-name hash/attr-num-use)))
                   ;; we see it the first time.
                   (setf (gethash attr-name hash/attr-num-use) 1)
                   ;; otherwise, increment that we saw another use of it.
                   (incf (gethash attr-name hash/attr-num-use)))))))

    ;; Now, for all entries in the hash/attr-num-use that are 1, those
    ;; are unique and can get a short name which can be used in the
    ;; incoming data DSL. There may be an more condenses why to do this if
    ;; I thought more, but meh, this is n^2, but that's ok, since the n's are
    ;; generally small.
    (let ((all-keys
           (loop :for k :being :the :hash-keys :in hash/attr-view
              :collecting k)))
      (maphash
       (lambda (attr-name use-count)
         (when (= use-count 1)
           ;; Find the formal-name in the hash/name->layout hash table which
           ;; uses this attribute name, there will be only one. Then, get the
           ;; datastore name out and assign the association in the
           ;; hash/attr-view for the short name.
           (let ((datastore-name (car (find attr-name all-keys :key #'cadr))))
             ;; This is safe to do here because I am not adding new keys
             ;; to the hashtable over which I am iterating.
             (setf (gethash attr-name hash/attr-view) datastore-name))))
       hash/attr-num-use))


    ;; all done, package everything up and return it!
    (%make-datastore-layout-set
     :attr-set attr-set
     :primitive-kind primitive-kind
     :datastore-layouts hash/name->layout
     :attribute-view hash/attr-view)))




(defun doit ()
  (let ((attr-set (make-attribute-set '(position :type :float :count 3)
                                      '(normal :type :float :count 3)
                                      '(uv :type :float :count 3)
                                      '(color :type :float :count 3)
                                      '(weight :type :float :count 3))))

    (make-layout-set attr-set :triangles
                     '(((:data-format :interleave)
                        (:binding-target :array-buffer)
                        (:usage-hint :static-draw))

                       (vertices (position normal uv)))

                     '(((:data-format :separate)
                        (:binding-target :array-buffer)
                        (:usage-hint :static-draw))

                       (colors (color))
                       (weights (weight)))
                     )))


;; (inspect (doit))
