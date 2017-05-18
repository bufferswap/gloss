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
  (:export #:make-attribute-set
           #:make-layout-set
           #:make-datastore-array-buffer
           #:commit-to-gpu
           #:attr-ref
           #:attr-group-byte-size
           #:attr-start-byte-offset))

(defpackage #:gloss.vao.example-0
  (:use #:cl
        #:alexandria
        #:gloss.common
        #:gloss.vao)
  (:export #:example-0))

(defpackage #:gloss.shader
  (:use #:cl
        #:alexandria
        #:gloss.common))
