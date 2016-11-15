(in-package :gloss.vao)

;;; VAO Layouts

(defstruct (datastore-properties (:constructor %make-datastore-properties)
                                 (:conc-name nil))
  data-format
  (binding-target :array-buffer)
  (usage-hint :static-draw))

(defstruct (layout-set (:constructor %make-layout-set)
                       (:conc-name nil))
  primitive
  (layouts (make-hash-table))
  attribute-set
  (attribute-view (make-hash-table))
  (attribute-usage (make-hash-table)))

(defstruct (layout (:constructor %make-layout)
                   (:conc-name nil))
  properties
  template)

(defun make-datastore-properties (properties)
  (apply #'%make-datastore-properties
         (reduce #'append properties :initial-value nil)))

(defun make-layout (properties template)
  (%make-layout
   :properties (make-datastore-properties properties)
   :template template))

;; TODO name this function better
(defun count-attribute-usage (attribute-name layout-set)
  (with-slots (attribute-usage) layout-set
    (if (nth-value 1 (gethash attribute-name attribute-usage))
        (incf (gethash attribute-name attribute-usage))
        (setf (gethash attribute-name attribute-usage) 1))))

;; TODO name this function better
(defun add-template-names (layout-set layout-name template)
  (with-slots (attribute-set attribute-view) layout-set
    (loop :for attr-name :in template
          :for name = (list layout-name attr-name)
          :do (unless (gethash attr-name (attributes attribute-set))
                (gloss-error :attribute-undefined attr-name))
              (setf (gethash name attribute-view) layout-name)
              (count-attribute-usage attr-name layout-set))))

;; TODO name this function better
(defun add-attribute-view-short-names (layout-set)
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
    (loop :for (properties . named-layouts) :in datastore-specs
          :do (loop :for (layout-name template) :in named-layouts
                    :do (setf (gethash layout-name (layouts layout-set))
                              (make-layout properties template))
                        (add-template-names layout-set layout-name template)))
    (add-attribute-view-short-names layout-set)
    layout-set))

(defun doit ()
  (let ((attr-set (make-attribute-set '(position :count 3)
                                      '(normal :count 3)
                                      '(uv :count 3)
                                      '(color :count 3)
                                      '(weight :count 3))))
    (make-layout-set
     attr-set :triangles
     '(((:data-format :interleave)
        (:binding-target :array-buffer)
        (:usage-hint :static-draw))
       (vertices (position normal uv)))
     '(((:data-format :separate)
        (:binding-target :array-buffer)
        (:usage-hint :static-draw))
       (colors (color))
       (weights (weight))))))
