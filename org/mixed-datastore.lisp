;;;; This piece of code is in a raw form using no helper libraries except for
;;;; cl-opengl and static-vectors (since gl-arrays suck a lot). The manually
;;;; allocated gl arrays are not properly freed and will leak memory. That
;;;; needs fixing. It needs a little help to get it ready to be used for
;;;; testing our stuff.

;; Here is the test image for the raw example program.
;; ./alignment.bmp

(ql:quickload "sdl2")
(ql:quickload "cl-opengl")
(ql:quickload "static-vectors")
(ql:quickload "ieee-floats")

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

;; Experimental stuff
(defun float->unsigned-int-static-vector (vec)
  (let ((sv (allocate-gl-typed-static-vector (length vec) :unsigned-int)))
    (dotimes (i (length vec))
      (setf (aref sv i)
            (coerce (ieee-floats::encode-float32 (aref vec i))
                    (gl-type->cl-type :unsigned-int))))
    sv))



(defun align-up-to (value power)
  "Align the value up to the next (expt 2 POWER) multiple if required."
  (let ((align (expt 2 power)))
    (logand (+ value (1- align)) (lognot (1- align)))))


;; This is slow.
(defun encode-and-explode (result value value-gl-type &key (align 1))
  (let ((endian2 (vector (byte 8 0) (byte 8 8)))
        (endian4 (vector (byte 8 0) (byte 8 8) (byte 8 16) (byte 8 24))))
    (ecase value-gl-type
      (:float
       (let ((val (ieee-floats::encode-float32 value)))
         (psetf (aref result 0) (ldb (aref endian4 0) val)
                (aref result 1) (ldb (aref endian4 1) val)
                (aref result 2) (ldb (aref endian4 2) val)
                (aref result 3) (ldb (aref endian4 3) val))
         ;; handle alignment
         (cond
           ((or (= align 1) (= align 2) (= align 4))
            ;; we're already aligned to these values
            (values result 4))
           ((= align 8)
            (psetf (aref result 4) 0
                   (aref result 5) 0
                   (aref result 6) 0
                   (aref result 7) 0)
            (values result 8))
           (t
            (error "encode-and-explode: bad alignment: ~A" align)))))

      ((:byte :unsigned-byte)
       (setf (aref result 0) value)
       ;; handle alignment
       (cond
         ((= align 1)
          (values result 1))
         ((= align 2)
          (psetf (aref result 1) 0)
          (values result 2))
         ((= align 4)
          (psetf (aref result 1) 0
                 (aref result 2) 0
                 (aref result 3) 0)
          (values result 4))
         ((= align 8)
          (psetf (aref result 1) 0
                 (aref result 2) 0
                 (aref result 3) 0
                 (aref result 4) 0
                 (aref result 5) 0
                 (aref result 6) 0
                 (aref result 7) 0)
          (values result 8))
         (t
          (error "encode-and-explode: bad alignment: ~A" align))))

      ((:short :unsigned-short :fixed)
       (psetf (aref result 0) (ldb (aref endian2 0) value)
              (aref result 1) (ldb (aref endian2 1) value))
       ;; handle alignment
       (cond
         ((or (= align 1) (= align 2))
          (values result 2))
         ((= align 4)
          (psetf (aref result 2) 0
                 (aref result 3) 0)
          (values result 4))
         ((= align 8)
          (psetf (aref result 2) 0
                 (aref result 3) 0
                 (aref result 4) 0
                 (aref result 5) 0
                 (aref result 6) 0
                 (aref result 7) 0)
          (values result 8))
         (t
          (error "encode-and-explode: bad alignment: ~A" align))))

      ((:int :unsigned-int)
       (psetf (aref result 0) (ldb (aref endian4 0) value)
              (aref result 1) (ldb (aref endian4 1) value)
              (aref result 2) (ldb (aref endian4 2) value)
              (aref result 3) (ldb (aref endian4 3) value))
       ;; handle alignment
       (cond
         ((or (= align 1) (= align 2) (= align 4))
          ;; we're already aligned to these values
          (values result 4))
         ((= align 8)
          (psetf (aref result 4) 0
                 (aref result 5) 0
                 (aref result 6) 0
                 (aref result 7) 0)
          (values result 8))
         (t
          (error "encode-and-explode: bad alignment: ~A" align))))

      (:half-float
       (error "encode-and-explode :half-float NIY"))

      (:double
       (error "encode-and-explode :double NIY")))))

;; Experimental: pack everything into an :unsigned-byte array.
(defun pattern->static-vector (pattern vec &key (align 1))
  (let* ((group-byte-size
          (apply #'+ (mapcar (lambda (the-type)
                               (max align (gl-type->byte-size the-type)))
                             pattern)))
         (left-over-elements (mod (length vec) (length pattern)))
         (group-num (/ (length vec) (length pattern)))
         (u8-size (* group-byte-size group-num))
         ;; We fill this array over and over with each new encoding we require
         ;; to stop memory churn.
         (encoding (make-array 8 :element-type '(unsigned-byte 8))))

    (unless (zerop left-over-elements)
      (error "There is an incorrect number of attribute elements."))

    (let ((result (allocate-gl-typed-static-vector u8-size :unsigned-byte)))

      (loop
         :for value-index :from 0 :below (length vec)
         :for value-gl-type = (nth (mod value-index (length pattern)) pattern)
         :with u8-start-index = 0
         :do

         (multiple-value-bind (value-bytes value-byte-size)
             (encode-and-explode encoding
                                 (aref vec value-index)
                                 value-gl-type
                                 :align align)

           #++(format
               t "Processing: type: ~A, value: ~A, u8-start-index: ~A, value-byte-size: ~A, value-bytes: ~A~%"
               value-gl-type
               (aref vec value-index)
               u8-start-index
               value-byte-size
               value-bytes)

           ;; copy byte by byte the exploded representation into the u8-array
           (loop
              :for u8-write-index
              :from u8-start-index
              :below (+ u8-start-index value-byte-size)

              :for value-byte-index :from 0 :by 1
              :do
              #++(format t "write byte ~A into location ~A~%"
                         (aref value-bytes value-byte-index)
                         u8-write-index)
              (setf (aref result u8-write-index)
                    (aref value-bytes value-byte-index)))


           (incf u8-start-index value-byte-size)))

      ;; Implement me.

      (values result group-num left-over-elements group-byte-size u8-size))))

(defun xxx ()
  (let ((sv
         (pattern->static-vector
	  ;;'((:float 3) (:unsigned-byte 2))
          '(:float :float :float :unsigned-byte :unsigned-byte)
          #(0.0 1.0 2.0 0 0
            3.0 4.0 5.0 1 1
            6.0 7.0 8.0 2 2
            9.0 10.0 11.0 3 3))))

    (format t "The u8 vector is: ~A~%" sv)

    (static-vectors:free-static-vector sv)))


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

               (texunit (load-bmp-to-gl-texture "alignment.bmp"))

               ;; Two distinct triangles forming a square around the origin.
               (glverts (;;float-vector->static-vector
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
               (glvert-colors (float-vector->static-vector
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
               (ebo-glelems (ushort-vector->static-vector
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
          ;; Bind the VAO
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (gl:bind-vertex-array vao)

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; bind interleaved position/uv datastore vbo into vao here
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (gl:bind-buffer :array-buffer vbo)
          ;; position/uv data -> GPU
          ;; TODO: Fix to compute the right length, this jsut happens to work.
          (%gl:buffer-data :array-buffer
                           (* (length glverts)
                              (gl-type->byte-size :unsigned-byte))
                           (static-vectors:static-vector-pointer glverts)
                           :static-draw)

          ;; Free the memory, don't need it anymore.
          (static-vectors:free-static-vector glverts)
          (setf glverts nil)

          ;; https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_03

          ;; Enable Position Attribute Data and associate with layout position
          ;; in shader
          (gl:enable-vertex-attrib-array pos-attr)
          (gl:vertex-attrib-pointer
           pos-attr 3 :float :false
           (+
            ;; 3 for position
            (* 3 (cffi:foreign-type-size :float))
            ;; 2 for uv
            (* 2 (cffi:foreign-type-size :unsigned-char)))
           0)

          ;; Enable UV Attribute Data and associate with layout position in
          ;; shader
          (gl:enable-vertex-attrib-array uv-attr)
          (gl:vertex-attrib-pointer
           uv-attr 2 :unsigned-byte :true
           (+
            ;; 3 for position
            (* 3 (cffi:foreign-type-size :float))
            ;; 2 for uv
            (* 2 (cffi:foreign-type-size :unsigned-char)))
           ;; start of first uv after position.
           (* 3 (cffi:foreign-type-size :float)))

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; bind separate color attribute datastore into vao here
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; bind vbo into vao here
          (gl:bind-buffer :array-buffer vbo-color)
          ;; color data -> GPU
          (%gl:buffer-data :array-buffer
                           (* (length glvert-colors)
                              (gl-type->byte-size :float))
                           (static-vectors:static-vector-pointer glvert-colors)
                           :static-draw)
          (static-vectors:free-static-vector glvert-colors)
          (setf glvert-colors NIL)

          ;; Enable Color Data and associate with layout position in shader.
          (gl:enable-vertex-attrib-array color-attr)
          (gl:vertex-attrib-pointer color-attr 4 :float :false
                                    ;; attempt explicit stride here.
                                    (* 4 (cffi:foreign-type-size :float)) 0)

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; bind the index array
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (gl:bind-buffer :element-array-buffer ebo)
          (%gl:buffer-data :element-array-buffer
                           (* (length ebo-glelems)
                              (gl-type->byte-size :unsigned-short))
                           (static-vectors:static-vector-pointer ebo-glelems)
                           :static-draw)

          ;; free it cause we don't need it anymore.
          (static-vectors:free-static-vector ebo-glelems)
          (setf ebo-glelems nil)

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
