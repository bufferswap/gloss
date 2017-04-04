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
	   #:make-native-datastore
	   #:commit-to-gpu
	   #:attr-ref
	   #:attr-group-byte-size))

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
