(in-package :gloss.vao)

(defstruct (attr (:constructor %make-attr))
  (type :float)
  (count 1)
  (normalizep nil)
  (divisor 0)
  (accessors nil))

(defun make-attr (attr-set attr-name &rest attr-data)
  (if (nth-value 1 (gethash attr-name attr-set))
      (error "Attribute ~A is defined more than once." attr-name)
      (apply #'%make-attr attr-data)))

(defun make-attr-set (&rest attr-specs)
  (let ((attr-set (make-hash-table)))
    (loop :for (name . data) :in attr-specs
          :for attr = (apply #'make-attr attr-set name data)
          :do (setf (gethash name attr-set) attr))
    attr-set))

;; type-checking validity table
;; this is just to remind what needs to be type-checked later on
(defvar *attr-validity-table*
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
