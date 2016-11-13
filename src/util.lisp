(in-package :gloss.util)

(define-condition gloss-error (error)
  ((message :reader gloss-error-message
            :initarg :message)
   (value :reader gloss-error-value
          :initarg :value
          :initform nil)
   (kind :reader gloss-error-kind
         :initarg :kind
         :initform :gloss-kind-unknown)))

(defmethod print-object ((object gloss-error) stream)
  (print-unreadable-object (object stream :type t)
    (let ((*print-pretty* t)
          (*print-circle* nil)
          (*print-escape* nil)
          (*print-gensym* nil)
          (*print-level* nil)
          (*print-length* nil)
          (*print-lines* nil)
          (*print-miser-width* nil)
          (*print-right-margin* most-positive-fixnum))
      (apply #'format stream
             (concatenate 'string "[Kind: ~S]: "
                          (gloss-error-message object))
             (gloss-error-kind object)
             (gloss-error-value object)))))

(defgeneric gloss-message (kind))

(defmethod gloss-message (kind)
  "An unknown error has occurred.")

(defun gloss-error (kind &rest value)
  (error 'gloss-error
         :kind kind
         :message (gloss-message kind)
         :value (copy-seq value)))
