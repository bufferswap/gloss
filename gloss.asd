(defsystem #:gloss
  :name "GLOSS"
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :version "0.1"
  :license "MIT"
  :description "A system aiming to simplify usage of the modern OpenGL interface."
  :depends-on (#:alexandria
               #:sdl2
               #:ieee-floats
               #:static-vectors
               #:cl-opengl)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:module "common"
                :components
                ((:file "util")
                 (:file "conditions")))
               (:module "vao"
                :components
                ((:file "package")
                 (:file "conditions")
                 (:file "attributes")
                 (:file "layout")
                 (:file "vao")
                 (:file "array-ops")
                 (:file "datastore")
		 (:file "datastore-array-buffer")
		 (:file "datastore-atomic-counter-buffer")
		 (:file "datastore-copy-buffer")
		 (:file "datastore-dispatch-indirect-buffer")
		 (:file "datastore-draw-indirect-buffer")
		 (:file "datastore-element-array-buffer")
		 (:file "datastore-pixel-buffer")
		 (:file "datastore-query-buffer")
		 (:file "datastore-shader-storage-buffer")
		 (:file "datastore-texture-buffer")
		 (:file "datastore-transform-feedback-buffer")
		 (:file "datastore-uniform-buffer")
                 (:file "test")
                 (:file "example-0")))
               (:module "shader"
                :components
                ((:file "shader")))))
