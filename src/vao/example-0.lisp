(in-package #:gloss.vao.example-0)

;;;; This piece of code is in a raw form using no helper libraries except for
;;;; cl-opengl and static-vectors (since gl-arrays suck a lot). The manually
;;;; allocated gl arrays are not properly freed and will leak memory. That
;;;; needs fixing. It needs a little help to get it ready to be used for
;;;; testing our stuff.

;; Here is the test image for the raw example program.
;; ./alignment.bmp

;;(ql:quickload "sdl2")
;;(ql:quickload "cl-opengl")
;;(ql:quickload "static-vectors")
;;(ql:quickload "ieee-floats")

(defparameter *vert-shader-src* "
#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 uv;
layout(location = 2) in vec4 color;

out varyings {
    vec2 uv;
    vec4 color;
} o;

uniform mat4 projection;

void main() {
       o.uv = uv;
       o.color = color;
       vec3 disp = vec3(10.0, 10.0, 0.0);
       gl_Position = projection * vec4(position + disp * gl_InstanceID, 1.0);
}
")

(defparameter *frag-shader-src* "
#version 330 core

layout(location = 0) out vec4 outputColor;

in varyings {
    vec2 uv;
    vec4 color;
} i;

uniform sampler2D texunit;

void main() {
    vec2 flipped_uv = vec2(i.uv.x, 1.0 - i.uv.y);
    outputColor = texture(texunit, flipped_uv) * i.color;
}
")

(defmacro with-main (&body body)
  `(sdl2:make-this-thread-main
    (lambda ()
      #+sbcl (sb-int:with-float-traps-masked (:invalid) ,@body)
      #-sbcl ,@body)))

(defun transpose-4x4-matrix (mat)
  (let ((new-mat (copy-seq mat)))
    (loop for col below 4 do
         (loop for row below col do
              (rotatef (elt new-mat (+ (* row 4) col))
                       (elt new-mat (+ (* col 4) row)))))
    new-mat))

;; Stuff for static vectors....

(defun gl-type->cl-type (gl-type)
  (ecase gl-type
    (:float 'single-float)
    (:byte '(integer -128 127))
    (:unsigned-byte '(integer 0 255))
    (:short '(integer -32768 32767))
    (:unsigned-short '(integer 0 65535))
    (:int '(integer −2147483648 2147483647))
    (:unsigned-int '(integer 0 4294967295))
    (:fixed '(integer 0 65535)) ;; 16.16 fixed point integer in 32-bits of space
    (:half-float '(integer 0 65535)) ;; half float in 16 bits of space.
    (:double 'double-float)))

(defun gl-type->byte-size (gl-type)
  (ecase gl-type
    (:float 4)
    (:byte 1)
    (:unsigned-byte 1)
    (:short 2)
    (:unsigned-short 2)
    (:int 4)
    (:unsigned-int 4)
    (:fixed 2) ;; 16.16 fixed point integer in 32-bits of space
    (:half-float 2) ;; half float in 16 bits of space.
    (:double 8)))

(defun allocate-gl-typed-static-vector (len gl-type)
  (static-vectors:make-static-vector
   len :element-type (gl-type->cl-type gl-type)))

(defun vector->static-vector (vec gl-type)
  (let* ((cl-type (gl-type->cl-type gl-type))
         (sv (allocate-gl-typed-static-vector (length vec) gl-type)))
    (dotimes (i (length vec))
      (setf (aref sv i) (coerce (aref vec i) cl-type)))
    sv))


(defun byte-vector->static-vector (vec)
  (vector->static-vector vec :byte))

(defun ubyte-vector->static-vector (vec)
  (vector->static-vector vec :unsigned-byte))

(defun short-vector->static-vector (vec)
  (vector->static-vector vec :short))

(defun ushort-vector->static-vector (vec)
  (vector->static-vector vec :unsigned-short))

(defun int-vector->static-vector (vec)
  (vector->static-vector vec :int))

(defun uint-vector->static-vector (vec)
  (vector->static-vector vec :unsigned-int))

(defun float-vector->static-vector (vec)
  (vector->static-vector vec :float))

(defun double-vector->static-vector (vec)
  (vector->static-vector vec :double))

(defun align-up-to (value power)
  "Align the value up to the next (expt 2 POWER) multiple if required."
  (let ((align (expt 2 power)))
    (logand (+ value (1- align)) (lognot (1- align)))))


(defun make-ortho-projection-matrix (left right top bottom near far)
  ;; SoraFirestorm: This is what type of array the prtho projection must be
  ;; in order to give it to gl:uniform-matrix-4fv. Don't use a VECTOR.
  (make-array
   16
   :element-type 'single-float
   :initial-contents
   (mapcar
    (lambda (x) (coerce x 'single-float))
    (list (/ 2.0 (- right left)) 0 0 (- (/ (+ right left) (- right left)))
          0 (/ 2.0 (- top bottom)) 0 (- (/ (+ top bottom) (- top bottom)))
          0 0 (/ -2.0 (- far near)) (- (/ (+ far near) (- far near)))
          0 0 0 1))))

(defun build-shader (type src)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (if (gl:get-shader shader :compile-status)
        shader
        (error "Shader compliation failed: ~a"
               (gl:get-shader-info-log shader)))))

(defun build-shader-program (&rest shaders)
  (let ((shader-program (gl:create-program)))
    (dolist (shader shaders) (gl:attach-shader shader-program shader))
    (gl:bind-frag-data-location shader-program 0 "outputColor")
    (gl:link-program shader-program)
    (if (gl:get-program shader-program :link-status)
        shader-program
        (error "Shader program linking failed: ~a"
               (gl:get-program-info-log shader-program)))))

(defun load-bmp-to-gl-texture (path)
  (let ((sdl-surf (sdl2:load-bmp path))
        (gltex (gl:gen-texture)))
    (gl:bind-texture :texture-2d gltex)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-image-2d :texture-2d 0 :rgba
                     (sdl2:surface-width sdl-surf)
                     (sdl2:surface-height sdl-surf)
                     0 :rgba :unsigned-byte (sdl2:surface-pixels sdl-surf))
    (format t "Loaded image ~A (h: ~A, w: ~A)~%"
            path (sdl2:surface-width sdl-surf) (sdl2:surface-height sdl-surf))
    (sdl2:free-surface sdl-surf)
    gltex))

(defun example-0-layout ()
  "Return a layout-set filled with attributes suitable for the datastore tests."
  (let ((attr-set (make-attribute-set
                   '(position :type :float :count 3 :accessors (px py pz))
                   '(rgba :type :float :count 4 :accessors (r g b a))
                   '(uv :type :unsigned-byte :count 2 :accessors (uvx uvy))
                   '(index :type :unsigned-short :count 1 :accessors (elem)))))
    (make-layout-set
     attr-set :triangles
     ;; Note: Datastore names must be unique.
     '(((:data-format :interleave)
        (:binding-target :array-buffer)
        (:usage-hint :static-draw)
        (:align T))
       (vertex (position uv)))

     '(((:data-format :separate)
        (:binding-target :array-buffer)
        (:usage-hint :static-draw)
        (:align T))
       (color (rgba)))

     '(((:data-format :separate)
        (:binding-target :element-array-buffer)
        (:usage-hint :static-draw)
        ;; Figure this out, why must this be NIL? Can I not align
        ;; :element-array-buffer data?
        (:align NIL))
       (indicies (index))))))


(defun main (&optional (method-choice :draw-arrays))
  (sdl2:with-init (:video)
    (sdl2:gl-set-attrs
     :context-profile-mask sdl2-ffi:+SDL-GL-CONTEXT-PROFILE-CORE+
     :context-major-version 3
     :context-minor-version 3)
    (sdl2:with-window (win :title "OGL" :w 800 :h 600 :flags '(:shown :opengl))
      (sdl2:gl-set-attr :doublebuffer 1)
      (sdl2:with-gl-context (glc win)
        (sdl2:gl-make-current win glc)
        (sdl2:gl-set-swap-interval 1)
        (gl:viewport 0 0 800 600)
        (gl:point-size 10)
        (gl:clear-color 0 0 0 1)

        (gl:active-texture :texture0)

        ;; for texture blending
        (gl:enable :blend)
        (gl:blend-func :src-alpha :one-minus-src-alpha)

        (let* ((vao (gl:gen-vertex-array))
               (vbo (gl:gen-buffer))
               (vbo-color (gl:gen-buffer))
               (ebo (gl:gen-buffer))

               (layout-set (example-0-layout))
               (ds-verts (make-native-datastore 'vertex
                                                layout-set
                                                :resizeable-p T))
               (ds-colors (make-native-datastore 'color
                                                 layout-set
                                                 :resizeable-p T))
               (ds-ebo-elems (make-native-datastore 'indicies
                                                    layout-set
                                                    :resizeable-p T))

               (texunit (load-bmp-to-gl-texture "alignment.bmp"))

               ;; Two distinct triangles forming a square around the origin.
               #++(glverts (;;float-vector->static-vector
                            ;;float->unsigned-int-static-vector
                            pattern->static-vector
                            '(:float :float :float :unsigned-byte :unsigned-byte)
                            #(-200.0 -200.0 0.0   0 0
                              200.0 200.0 0.0     255 255
                              -200.0 200.0 0.0    0 255

                              -200.0 -200.0 0.0   0 0
                              200.0 -200.0 0.0    255 0
                              200.0 200.0 0.0     255 255
                              )))

               ;; we intentionally put this in a seperate vbo.
               #++(glvert-colors (float-vector->static-vector
                                  #(1.0 1.0 1.0 1.0
                                    1.0 1.0 1.0 1.0
                                    1.0 1.0 1.0 0.0

                                    1.0 1.0 1.0 1.0
                                    1.0 1.0 1.0 0.0
                                    1.0 1.0 1.0 1.0)))

               ;; I can reuse indicies if I want to get the right thing.
               (glelems (ushort-vector->static-vector
                         #(0 1 2 3 4 5)))

               ;; store these indicies into the GPU so we can try
               ;; them out.  Use a different element ordering for
               ;; the verticies.
               #++(ebo-glelems (ushort-vector->static-vector
                                #(0 1 2 0 4 1)))

               (shader-program (build-shader-program
                                (build-shader :vertex-shader *vert-shader-src*)
                                (build-shader :fragment-shader *frag-shader-src*)))
               (pos-attr
                (gl:get-attrib-location shader-program "position"))

               (uv-attr
                (gl:get-attrib-location shader-program "uv"))

               (color-attr
                (gl:get-attrib-location shader-program "color"))

               (uniform-projection
                (gl:get-uniform-location shader-program "projection"))

               (uniform-texunit
                (gl:get-uniform-location shader-program "texunit"))

               (proj-matrix
                (make-ortho-projection-matrix
                 -400.0 400.0 300.0 -300.0 -1.0 1.0)))

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Fill the datastores
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;; ds-verts (fix to use whole accessor form)
          ;; triangle 1
          (setf (attr-ref ds-verts 'position :end) (vector -200.0 -200.0 0.0))
          (setf (attr-ref ds-verts 'uv :end) (vector 0 0))
          (setf (attr-ref ds-verts 'position :end) (vector 200.0 200.0 0.0))
          (setf (attr-ref ds-verts 'uv :end) (vector 255 255))
          (setf (attr-ref ds-verts 'position :end) (vector -200.0 200.0 0.0))
          (setf (attr-ref ds-verts 'uv :end) (vector 0 255))
          ;; triangle 2
          (setf (attr-ref ds-verts 'position :end) (vector -200.0 -200.0 0.0))
          (setf (attr-ref ds-verts 'uv :end) (vector 0 0))
          (setf (attr-ref ds-verts 'position :end) (vector 200.0 -200.0 0.0))
          (setf (attr-ref ds-verts 'uv :end) (vector 255 0))
          (setf (attr-ref ds-verts 'position :end) (vector 200.0 200.0 0.0))
          (setf (attr-ref ds-verts 'uv :end) (vector 255 255))

          ;; ds-colors
          ;; triangle 1
          (setf (attr-ref ds-colors 'rgba :end) (vector 1.0 1.0 1.0 1.0))
          (setf (attr-ref ds-colors 'rgba :end) (vector 1.0 1.0 1.0 1.0))
          (setf (attr-ref ds-colors 'rgba :end) (vector 1.0 1.0 1.0 1.0))
          ;; triangle 2
          (setf (attr-ref ds-colors 'rgba :end) (vector 1.0 1.0 1.0 1.0))
          (setf (attr-ref ds-colors 'rgba :end) (vector 1.0 1.0 1.0 1.0))
          (setf (attr-ref ds-colors 'rgba :end) (vector 1.0 1.0 1.0 1.0))

          ;; ds-ebo-elems
          ;; triangle 1
          (setf (attr-ref ds-ebo-elems 'index :end) (vector 0))
          (setf (attr-ref ds-ebo-elems 'index :end) (vector 1))
          (setf (attr-ref ds-ebo-elems 'index :end) (vector 2))
          ;; triangle 2
          (setf (attr-ref ds-ebo-elems 'index :end) (vector 0))
          (setf (attr-ref ds-ebo-elems 'index :end) (vector 4))
          (setf (attr-ref ds-ebo-elems 'index :end) (vector 1))

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Bind the VAO
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (gl:bind-vertex-array vao)

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; bind interleaved position/uv datastore vbo into vao here
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (gl:bind-buffer :array-buffer vbo)
          ;; position/uv data -> GPU
          (commit-to-gpu ds-verts)

          #++(%gl:buffer-data :array-buffer
                              (* (length glverts)
                                 (gl-type->byte-size :unsigned-byte))
                              (static-vectors:static-vector-pointer glverts)
                              :static-draw)

          ;; Free the memory, don't need it anymore.
          #++(static-vectors:free-static-vector glverts)
          #++(setf glverts nil)

          ;; https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_03

          ;; Enable Position Attribute Data and associate with layout position
          ;; in shader
          (gl:enable-vertex-attrib-array pos-attr)
          (gl:vertex-attrib-pointer
           pos-attr 3 :float :false
           (attr-group-byte-size ds-verts)
           #++(+
               ;; 3 for position
               (* 3 (cffi:foreign-type-size :float))
               ;; 2 for uv
               (* 2 (cffi:foreign-type-size :unsigned-char)))
           (attr-start-byte-offset ds-verts 'position)
           #++ 0)

          ;; Enable UV Attribute Data and associate with layout position in
          ;; shader
          (gl:enable-vertex-attrib-array uv-attr)
          (gl:vertex-attrib-pointer
           uv-attr 2 :unsigned-byte :true
           (attr-group-byte-size ds-verts)
           #++(+
               ;; 3 for position
               (* 3 (cffi:foreign-type-size :float))
               ;; 2 for uv
               (* 2 (cffi:foreign-type-size :unsigned-char)))
           ;; start of first uv after position.
           (attr-start-byte-offset ds-verts 'uv)
           #++(* 3 (cffi:foreign-type-size :float)))

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; bind separate color attribute datastore into vao here
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; bind vbo into vao here
          (gl:bind-buffer :array-buffer vbo-color)
          ;; color data -> GPU
          (commit-to-gpu ds-colors)
          #++(%gl:buffer-data :array-buffer
                              (* (length glvert-colors)
                                 (gl-type->byte-size :float))
                              (static-vectors:static-vector-pointer glvert-colors)
                              :static-draw)
          #++(static-vectors:free-static-vector glvert-colors)
          #++(setf glvert-colors NIL)

          ;; Enable Color Data and associate with layout position in shader.
          (gl:enable-vertex-attrib-array color-attr)
          (gl:vertex-attrib-pointer color-attr 4 :float :false
                                    (attr-group-byte-size ds-colors)
                                    ;; attempt explicit stride here.
                                    (attr-start-byte-offset ds-colors 'rgba)
                                    #++(* 4 (cffi:foreign-type-size :float))
                                    #++ 0)

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; bind the index array
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (gl:bind-buffer :element-array-buffer ebo)
          ;; elem data -> GPU
          (commit-to-gpu ds-ebo-elems)

          #++(%gl:buffer-data :element-array-buffer
                              (* (length ebo-glelems)
                                 (gl-type->byte-size :unsigned-short))
                              (static-vectors:static-vector-pointer ebo-glelems)
                              :static-draw)

          ;; free it cause we don't need it anymore.
          #++(static-vectors:free-static-vector ebo-glelems)
          #++(setf ebo-glelems nil)

          ;; Unbind the VAO
          (gl:bind-vertex-array 0)

          (format t "texunit ~A, pos-attr ~A, uv-attr ~A~%" texunit pos-attr
                  uv-attr)

          (format t "Using draw method: ~(~S~)~%" method-choice)

          (sdl2:with-event-loop (:method :poll)
            (:idle ()
                   (gl:clear :color-buffer)

                   ;; Not needed here unless you have multiple programs
                   (gl:use-program shader-program)

                   ;; Bind the vao we want to draw.
                   ;; We could do this outside of the loop since we only have 1.
                   (gl:bind-vertex-array vao)

                   ;; bind the texture we need.
                   (gl:active-texture :texture0)
                   (gl:bind-texture :texture-2d texunit)

                   ;; Needed here each time.
                   (gl:uniform-matrix-4fv uniform-projection proj-matrix t)

                   ;; Needed for texturing, we use :texture0 which is unit 0.
                   (gl:uniformi uniform-texunit 0)

                   ;; Draw the data (either way works, but each
                   ;; slightly differently).

                   (ecase method-choice
                     (:draw-arrays
                      ;; using draw arrays on the bound vbo
                      (gl:draw-arrays :triangles 0 6))

                     (:draw-arrays-instanced
                      ;; using draw instance arrays on the bound vbo
                      ;; Poke |3b| to get the cl-opengl symbol working.
                      (gl:draw-arrays-instanced :triangles 0 6 5))

                     (:draw-elements
                      ;; using indicies being passed in.
                      (gl:bind-buffer :element-array-buffer 0)
                      (%gl:draw-elements
                       :triangles 6
                       :unsigned-short
                       (static-vectors:static-vector-pointer glelems)))

                     (:draw-range-elements
                      ;; using indicies being passed in.
                      (gl:bind-buffer :element-array-buffer 0)
                      (%gl:draw-range-elements
                       :triangles 0 5
                       6
                       :unsigned-short
                       (static-vectors:static-vector-pointer glelems)))

                     (:draw-elements-instanced
                      ;; using indicies being passed in.
                      (gl:bind-buffer :element-array-buffer 0)
                      (%gl:draw-elements-instanced
                       :triangles 6
                       :unsigned-short
                       (static-vectors:static-vector-pointer glelems)
                       5))

                     (:element-array-index
                      ;; Using ebo
                      (gl:bind-buffer :element-array-buffer ebo)
                      (%gl:draw-elements
                       :triangles 6
                       :unsigned-short
                       (cffi:null-pointer)))

                     (:element-array-index-instanced
                      ;; Using ebo and instancing
                      (gl:bind-buffer :element-array-buffer ebo)
                      (%gl:draw-elements-instanced
                       :triangles 6 :unsigned-short
                       (cffi:null-pointer)
                       5)

                      ))

                   (sdl2:gl-swap-window win))
            (:quit ()
                   t))

          ;; Clean up memory.
          (static-vectors:free-static-vector glelems)

          (format t "Exiting.~%"))))))

(defun run-test (&optional (method-choice :draw-arrays))
  (with-main (main method-choice)))

(defun run-all-tests ()
  (run-test :draw-arrays)
  (run-test :draw-arrays-instanced)
  (run-test :draw-elements)
  (run-test :draw-range-elements)
  (run-test :draw-elements-instanced)
  (run-test :element-array-index)
  (run-test :element-array-index-instanced))


(defun example-0 ()
  (run-all-tests))
