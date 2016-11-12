(defsystem #:gloss
  :name "GLOSS"
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :version "0.1"
  :license "MIT"
  :description "A system aiming to simplify usage of the modern OpenGL interface."
  :depends-on (#:defpackage-plus
               #:alexandria)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "error")
               (:module "vao"
                :components
                ((:file "attributes")
                 (:file "layout")
                 (:file "vao")))
               (:module "shader"
                :components
                ((:file "shader")))))
