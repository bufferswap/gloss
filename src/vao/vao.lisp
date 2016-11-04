(in-package :gloss.vao)

;;; VAO Attributes

(defstruct (attribute (:constructor %make-attribute))
  (type :float)
  (count 1)
  (normalizep nil)
  (divisor 0)
  (accessors nil))

(defun make-attribute (attribute-set attribute-name &rest attribute-data)
  (if (nth-value 1 (gethash attribute-name attribute-set))
      (error "Attribute ~A is defined more than once." attribute-name)
      (apply #'%make-attribute attribute-data)))

(defun add-attributes (attribute-set &rest attribute-specs)
  (loop :for (name . data) :in attribute-specs
        :for attr = (apply #'make-attribute attribute-set name data)
        :do (setf (gethash name attribute-set) attr))
  attribute-set)

(defun remove-attributes (attribute-set &rest attribute-names)
  (loop :for name :in attribute-names
        :do (remhash name attribute-set))
  attribute-set)

(defun %make-attribute-set (&rest attribute-specs)
  (let ((attribute-set (make-hash-table)))
    (apply #'add-attributes attribute-set attribute-specs)
    attribute-set))

(defmacro make-attribute-set (options &body attribute-specs)
  (declare (ignore options))
  `(apply #'%make-attribute-set ',attribute-specs))

;; type-checking validity table
;; this is just to remind what needs to be type-checked later on
#++(defvar *attr-validity-table*
  '((:half-float . (:version 3.0))
    (:float . (:version 3.0))
    (:byte . (:version 3.0))
    (:unsigned-byte . (:version 3.0))
    (:short . (:version 3.0))
    (:unsigned-short . (:version 3.0))
    (:int . (:version 3.0))
    (:unsigned-int . (:version 3.0))
    (:int-2-10-10-10-rev . (:version 3.2 :count 4))
    (:unsigned-int-2-10-10-10-rev . (:version 3.2 :count 4))
    (:unsigned-int-10f-11f-11f-rev . (:version 4.4 :count 3))))

;; usage make-attribute-set is a macro that so far just allows defining
;; attribute specs the same way as its functional expansion,
;; %make-attribute-set, except without QUOTE'ing each spec.
#++(let ((attr-set
        (make-attribute-set ()
          (position :type :float :count 3 :accessors (px py pz))
          (normals :type :float :count 3 :accessors (nx ny nz))
          (uvs :type :float :count 3 :accessors (uvx uvy uvz)))))
  (add-attributes attr-set
                  '(some-attr-1 :type :byte :count 2)
                  '(some-attr-2 :type :unsigned-byte :count 2))
  (remove-attributes attr-set 'some-attr-1)
  attr-set)
