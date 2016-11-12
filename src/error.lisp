(in-package :gloss.error)

(define-condition gloss-error (error)
  ((message :accessor gloss-error-message
            :initarg :message
            :initform nil)
   (value :accessor gloss-error-value
          :initarg :value
          :initform nil)))

(defmethod print-object ((object gloss-error) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S~@[: ~S~]"
            (gloss-error-message object)
            (gloss-error-value object))))

(defun gloss-error (message &key value)
  (error 'gloss-error :message message :value value))
