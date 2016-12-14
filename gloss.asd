(defsystem #:gloss
  :name "GLOSS"
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :version "0.1"
  :license "MIT"
  :description "A system aiming to simplify usage of the modern OpenGL interface."
  :depends-on (#:alexandria
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
                ((:file "conditions")
                 (:file "attributes")
                 (:file "layout")
                 (:file "vao")
		 (:file "datastore")
                 (:file "test")))
               (:module "shader"
                :components
                ((:file "shader")))))
