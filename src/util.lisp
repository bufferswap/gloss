(in-package :gloss.util)

(define-condition gloss-error (error)
  ((message :reader gloss-error-message
            :initarg :message)
   (value :reader gloss-error-value
          :initarg :value
          :initform nil)))

(defmethod print-object ((object gloss-error) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S~@[: ~S~]"
            (gloss-error-message object)
            (gloss-error-value object))))

(defun gloss-error (message &key value)
  (error 'gloss-error :message message :value value))

(define-condition gloss-warning (warning)
  ((message :reader gloss-warning-message
            :initarg :message)
   (value :reader gloss-warning-value
          :initarg :value
          :initform nil)))

(defmethod print-object ((object gloss-warning) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S~@[: ~S~]"
            (gloss-warning-message object)
            (gloss-warning-value object))))

(defun gloss-warn (message &key value)
  (warn 'gloss-warning :message message :value value))
