(in-package :gloss.common)

(define-condition gloss-error (error)
  ((kind :reader gloss-error-kind
         :initarg :kind
         :initform :unknown-kind)
   (message :reader gloss-error-message
            :initarg :message)
   (value :reader gloss-error-value
          :initarg :value
          :initform nil)))

(defmethod print-object ((object gloss-error) stream)
  (print-unreadable-object (object stream :type t)
    (apply #'format stream
           (concatenate 'string "[~A]~&"
                        (gloss-error-message object))
           (gloss-error-kind object)
           (gloss-error-value object))))

(defgeneric gloss-message (kind)
  (:method (kind)
    "An unknown error has occurred."))

(defun gloss-error (kind &rest value)
  (error 'gloss-error
         :kind kind
         :message (gloss-message kind)
         :value (copy-seq value)))
