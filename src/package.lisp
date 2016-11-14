(in-package :cl-user)

(defpackage #:gloss.common
  (:use #:cl
        #:alexandria)
  (:export #:gloss-error
           #:gloss-message
           #:select-if))

(defpackage #:gloss.vao
  (:use #:cl
        #:alexandria
        #:gloss.common)
  (:export #:make-attribute-set))

(defpackage #:gloss.shader
  (:use #:cl
        #:alexandria
        #:gloss.common))
