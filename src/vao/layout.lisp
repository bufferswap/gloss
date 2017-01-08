(in-package :gloss.vao)

;;; VAO Layouts

(defstruct (datastore-properties (:constructor %make-datastore-properties)
                                 (:conc-name nil))
  data-format
  (align T)
  (binding-target :array-buffer)
  (usage-hint :static-draw))

(defstruct (layout (:constructor %make-layout)
                   (:conc-name nil))
  properties
  template)

(defstruct (layout-set (:constructor %make-layout-set)
                       (:conc-name nil))
  primitive
  (layouts (make-hash-table))
  attribute-set
  (attribute-view (make-hash-table :test #'equal))
  (attribute-usage (make-hash-table)))

(defun make-datastore-properties (properties)
  (apply #'%make-datastore-properties
         (reduce #'append properties :initial-value nil)))

(defun make-layout (properties template)
  (%make-layout
   :properties (make-datastore-properties properties)
   :template template))

(defun count-attribute-usage (attribute-name layout-set)
  (with-slots (attribute-usage) layout-set
    (symbol-macrolet ((name-ref (gethash attribute-name attribute-usage)))
      (if (nth-value 1 name-ref)
          (incf name-ref)
          (setf name-ref 1)))))

(defun add-datastore-template (layout-set datastore-name datastore-template)
  (with-slots (attribute-set attribute-view) layout-set
    (loop :for attr-name :in datastore-template
       :for formal-name = (list datastore-name attr-name)
       :do (unless (gethash attr-name (attributes attribute-set))
             (gloss-error :attribute-undefined attr-name))
       (setf (gethash formal-name attribute-view) datastore-name)
       (count-attribute-usage attr-name layout-set))))

(defun add-named-datastore-layout (layout-set datastore-name
                                   datastore-properties datastore-template)
  (setf (gethash datastore-name (layouts layout-set))
        (make-layout datastore-properties datastore-template))
  (add-datastore-template layout-set datastore-name datastore-template))

(defun generate-valid-attribute-short-names (layout-set)
  (with-slots (attribute-view attribute-usage) layout-set
    (let* ((all-keys (hash-table-keys attribute-view)))
      (maphash
       (lambda (attr-name use-count)
         (when (= use-count 1)
           (setf (gethash attr-name attribute-view)
                 (car (find attr-name all-keys :key #'cadr)))))
       attribute-usage))))

(defun make-layout-set (attribute-set primitive &rest datastore-specs)
  (let ((layout-set (%make-layout-set :attribute-set attribute-set
                                      :primitive primitive)))
    (loop :for (datastore-properties . named-layouts) :in datastore-specs
       :do (loop :for (datastore-name datastore-template) :in named-layouts
              :do (add-named-datastore-layout
                   layout-set datastore-name
                   datastore-properties datastore-template)))
    (generate-valid-attribute-short-names layout-set)
    layout-set))

(defun lookup-named-layout (datastore-name layout-set)
  (gethash datastore-name (layouts layout-set)))

(defun doit ()
  (let ((attr-set (make-attribute-set '(position :count 3)
                                      '(normal :count 3)
                                      '(uv :type :short :count 3)
                                      '(color :count 3)
                                      '(weight :type :short :count 3))))
    (make-layout-set
     attr-set :triangles
     '(((:data-format :interleave)
        (:binding-target :array-buffer)
        (:usage-hint :static-draw)
        (:align T))
       (vertices (position normal uv)))
     '(((:data-format :separate)
        (:binding-target :array-buffer)
        (:usage-hint :static-draw)
        (:align T))
       (colors (color))
       (weights (weight))))))
