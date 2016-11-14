(in-package :gloss.vao)

(defclass vao ()
  ((%index :accessor vao-index
           :initarg :vao-index
           :initform NIL)
   (%layout-set :accessor vao-layout-set
                :initarg :vao-layout-set
                :initform NIL)
   (%datastore-manager :accessor vao-datastore-manager
                       :initarg :vao-datastore-manager
                       :initform NIL)))

;; This requires an OpenGL context when called.
(defun make-vao (layout-set)
  (apply #'make-instance 'vao
         :vao-index (gl:gen-vertex-arrays)
         :vao-layout-set layout-set))

(defun vao-bind (vao)
  (gl:bind-vertex-array (vao-index vao))
  vao)

(defun vao-unbind (vao)
  (gl:bind-vertex-array 0)
  vao)

(defun vao-destroy (vao)
  (gl:delete-vertex-arrays (vao-index vao))
  (setf (vao-index vao) NIL)
  vao)

(defun vao-valid-p (vao)
  (vao-index vao))
