(in-package :cl-user)

(defpackage #:gloss.vao
  (:use #:cl
        #:alexandria))

(defpackage #:gloss.shader
  (:use #:cl
        #:alexandria))

(defpackage+-1:defpackage+ #:gloss
  (:use #:cl
        #:alexandria)
  (:inherit #:gloss.vao
            #:gloss.shader))
