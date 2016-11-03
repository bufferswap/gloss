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

(defun attr-type-valid-p (type gl-version)
  "Check whether the given attribute type and OpenGL version is legal."
  (let ((validity-table
          '((:half-float . 3.0)
            (:float . 3.0)
            (:byte . 3.0)
            (:unsigned-byte . 3.0)
            (:short . 3.0)
            (:unsigned-short . 3.0)
            (:int . 3.0)
            (:unsigned-int . 3.0)
            (:int-2-10-10-10-rev . 3.2)
            (:unsigned-int-2-10-10-10-rev . 3.2)
            (:unsigned-int-10f-11f-11f-rev . 4.4))))
    (>= (cdr (assoc type validity-table)) gl-version)))
