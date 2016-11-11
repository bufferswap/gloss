(in-package :gloss.vao)

;;; VAO Layouts

(defstruct (datastore-properties (:constructor %make-datastore-properties)
                                 (:conc-name nil))
  datastore-format
  (binding-target :array-buffer)
  (usage-hint :static-draw))

(defstruct (datastore-layout-set (:constructor %make-datastore-layout-set)
                       (:conc-name nil))
  (datastore-layouts (make-hash-table))
  (attribute-view (make-hash-table)))

(defstruct (datastore-layout (:constructor %make-datastore-layout)
                             (:conc-name nil))
  datastore-properties
  datastore-template)
