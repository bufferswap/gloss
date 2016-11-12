(in-package :cl-user)

(defpackage #:gloss.error
  (:use #:cl)
  (:export #:gloss-error))

(defpackage #:gloss.vao
  (:use #:cl
        #:alexandria
        #:gloss.error)
  (:export #:make-attribute-set))

(defpackage #:gloss.shader
  (:use #:cl
        #:alexandria
        #:gloss.error))

(defpackage+-1:defpackage+ #:gloss
  (:use #:cl
        #:alexandria)
  (:inherit #:gloss.vao
            #:gloss.shader))
