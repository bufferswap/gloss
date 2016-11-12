(in-package :cl-user)

(defpackage #:gloss.util
  (:use #:cl)
  (:export #:gloss-error
           #:gloss-warn))

(defpackage #:gloss.vao
  (:use #:cl
        #:alexandria
        #:gloss.util)
  (:export #:make-attribute-set))

(defpackage #:gloss.shader
  (:use #:cl
        #:alexandria
        #:gloss.util))

(defpackage+-1:defpackage+ #:gloss
  (:use #:cl
        #:alexandria
        #:gloss.util)
  (:inherit #:gloss.vao
            #:gloss.shader))
