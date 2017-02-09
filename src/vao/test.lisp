(in-package :gloss.vao)

;;;; some math functions only used for internal tests.

(defun cross-product (u v)
  "A simple cross product for U x V for use within the tests."
  (vector (- (* (aref u 1) (aref v 2))
             (* (aref u 2) (aref v 1)))

          (- (* (aref u 2) (aref v 0))
             (* (aref u 0) (aref v 2)))

          (- (* (aref u 0) (aref v 1))
             (* (aref u 1) (aref v 0)))))

(defun vect (p0 p1)
  "Construct a vector from P0 to P1."
  (vector (- (aref p1 0) (aref p0 0))
          (- (aref p1 1) (aref p0 1))
          (- (aref p1 2) (aref p0 2))))



;;;; attribute-set tests


(defun test-mls/pass/no-locations ()
  (make-attribute-set
   '(position :type :float :count 3)
   '(normal :type :float :count 3)
   '(uv :type :float :count 2)))

(defun test-mls/pass/all-good-locations ()
  (make-attribute-set
   '(position :type :float :count 3 :location 0)
   '(normal :type :float :count 3 :location 1)
   '(uv :type :float :count 2 :location 2)))

(defun test-mls/fail/duplicate-attrs ()
  (make-attribute-set
   '(position :type :float :count 3)
   '(normal :type :float :count 3)
   '(uv :type :float :count 3)
   '(normal :type :float :count 3)
   '(position :type :float :count 2)))

(defun test-mls/fail/partial-locations ()
  (make-attribute-set
   '(position :type :float :count 3)
   '(normal :type :float :count 3 :location 0)
   '(uv :type :float :count 3)))

(defun test-mls/fail/bad-type-location ()
  (make-attribute-set
   '(position :type :float :count 3 :location 0)
   '(normal :type :float :count 3 :location 'bar)
   '(uv :type :float :count 3 :location 'foo)))

(defun test-mls/fail/bad-range-location ()
  (make-attribute-set
   '(position :type :float :count 3 :location 0)
   '(normal :type :float :count 3 :location 1)
   '(uv :type :float :count 3 :location -5)))

(defun test-mls ()
  (test-mls/pass/no-locations)
  (test-mls/pass/all-good-locations)
  (test-mls/fail/partial-locations)
  (test-mls/fail/bad-type-location)
  (test-mls/fail/bad-range-location))


;;;; layout-set tests



;;;; datastore tests


(defun test-0 ()
  "Test vec->static-vector/unsigned-byte."
  (let ((sv (allocate-gl-typed-static-vector 32 :unsigned-byte))
        (tests `( ;; gl-type write-index in-vec read-index
                 (:unsigned-byte 0 ,(vector 0 1 2 3 255) 0)
                 (:byte 0 ,(vector -2 -1 0 1 2) 0)
                 (:unsigned-short 0 ,(vector 0 1 2 65534 65535) 0)
                 (:short 0 ,(vector -32768 -32767 -1 0 1 32766 32767) 0)
                 (:int 0 ,(vector -2147483648 -1 0 1 2147483647) 0)
                 (:unsigned-int 0 ,(vector 0 1 2 4294967295) 0)
                 (:fixed 0 ,(vector -2.0 -1.5 -1.0 0 1.0 1.5 2.0) 0)
                 (:half-float 0 ,(vector -65504.0 -2 1.5 0 1.5 2.0 65504.0) 0)
                 (:float 0 ,(vector -2 -1.5 0 1.5 2.0) 0)))
        (passed 0))

    (flet ((clear (svec)
             ;; clear out-vec...
             (loop :for idx :below (length svec) :do
                (setf (aref svec idx) 0)))

           (stored (gl-type in-vec read-index sv write-index)
             (format t "Stored ~A:~%in-vec = ~A from index ~A~%sv= into ~A~%at byte index ~A~%"
                     gl-type in-vec read-index sv write-index)))

      ;; test each gl-type
      (loop :for (gl-type read-index in-vec write-index) :in tests :do
         (clear sv)
         ;; first, fill it.
         (vec->sv/unsigned-byte gl-type sv write-index in-vec read-index
                                (length in-vec))
         ;; then emit what we did.
         (stored gl-type in-vec read-index sv write-index)
         ;; Then extract it
         (let ((out-vec (sv/unsigned-byte->vec gl-type (length in-vec) sv 0)))

           (format t "out-vec is ~A~%" out-vec)

           ;; and compare
           (cond
             ;; = is not entirely appropriate for for floating point values,
             ;; and definitely not appropriate for :fixed, since :fixed is
             ;; designed to lose precision.
             ((every #'identity (map 'vector #'= in-vec out-vec))
              (format t "gl-type: ~A PASSED~%" gl-type)
              (incf passed))
             (t
              (format t "gl-type: ~A FAILUE~%" gl-type)))

           (format t "~%")))

      (format t "Out of ~A tests, ~A passed.~%" (length tests) passed)


      (static-vectors:free-static-vector sv))))

(defun test-ds-layout ()
  "Return a layout-set filled with attributes suitable for the datastore tests."
  (let ((attr-set (make-attribute-set
                   '(position :count 3 :accessors (px py pz))
                   '(normal :count 3 :accessors (nx ny nz))
                   '(uv :type :half-float :count 2 :accessors (uvx uvy)))))
    (make-layout-set
     attr-set :triangles
     ;; Note: Datastore names must be unique.
     '(((:data-format :interleave)
        (:binding-target :array-buffer)
        (:usage-hint :static-draw)
        (:align T))
       (vertex (position normal uv)))

     '(((:data-format :separate)
        (:binding-target :array-buffer)
        (:usage-hint :static-draw)
        (:align T))
       (svertex (position))
       (normal (normal))
       (uv (uv)))

     '(((:data-format :block)
        (:binding-target :array-buffer)
        (:usage-hint :static-draw)
        (:align T))
       (bvertex (position normal uv))))))


(defun test-1 ()
  (let* ((layout-set (test-ds-layout))
         (ds (make-native-datastore 'vertex layout-set :size 4)))
    (setf (attr-ref ds 'position 0) #(1.0 1.0 1.0))
    (setf (attr-ref ds 'normal 0) #(4.0 5.0 6.0))
    (setf (attr-ref ds 'uv 0) #(0 0))
    (format t "position[0]: ~A~%" (attr-ref ds 'position 0))
    (format t "normal[0]: ~A~%" (attr-ref ds 'normal 0))
    (format t "uv[0]: ~A~%" (attr-ref ds 'uv 0))
    (loop
       :for i :from 0 :below 3
       :for acc :in '(x y z) :do
       (format t "position[0][~A]: ~A~%" i (attr-ref ds 'position 0 i))
       (format t "position[0][~A]: ~A~%" acc (attr-ref ds 'position 0 acc)))
    (format t "position[0][x,y]: ~A~%" (attr-ref ds 'position 0 'x 'y))
    (format t "position[0][y,z]: ~A~%" (attr-ref ds 'position 0 'y 'z))
    (format t "position[0][x,y,y]: ~A~%" (attr-ref ds 'position 0 'x 'y 'y))

    (format t "Component write test.~%")
    (setf (attr-ref ds 'position 0 0) #(42.0))
    (setf (attr-ref ds 'position 0 1) #(43.0))
    (setf (attr-ref ds 'position 0 2) #(44.0))
    (format t "position[0]: ~A~%" (attr-ref ds 'position 0))
    (setf (attr-ref ds 'position 0 'x) #(45.0))
    (setf (attr-ref ds 'position 0 'y) #(46.0))
    (setf (attr-ref ds 'position 0 'z) #(47.0))
    (format t "position[0]: ~A~%" (attr-ref ds 'position 0))
    (setf (attr-ref ds 'position 0 0 1) #(48.0 49.0))
    (format t "position[0]: ~A~%" (attr-ref ds 'position 0))
    (setf (attr-ref ds 'position 0 0 2) #(50.0 51.0))
    (format t "position[0]: ~A~%" (attr-ref ds 'position 0))
    (setf (attr-ref ds 'position 0 0 1 2) #(50.0 51.0 52))
    (format t "position[0]: ~A~%" (attr-ref ds 'position 0))
    (setf (attr-ref ds 'position 0 'x 'y) #(100.0 101.0))
    (format t "position[0]: ~A~%" (attr-ref ds 'position 0))
    (setf (attr-ref ds 'position 0 'x 'z) #(102.0 103.0))
    (format t "position[0]: ~A~%" (attr-ref ds 'position 0))
    (setf (attr-ref ds 'position 0 'x 'y 'z) #(400.0 401.0 402.0))
    (format t "position[0]: ~A~%" (attr-ref ds 'position 0))

    (inspect ds)

    (destroy-datastore ds)))


(defun test-2 ()
  (let* ((num-tris 1024)
         (layout-set (test-ds-layout))
         (ds (make-native-datastore 'vertex layout-set
                                    :size (* num-tris 3)
                                    :resizeable-p NIL)))

    ;; Let's make some triangles in a 0.0 - 1.0 range with each vertex
    (loop :for i :below num-tris :do
       (let* ((p0 (vector (random 1.0) (random 1.0) (random 1.0)))
              (p1 (vector (random 1.0) (random 1.0) (random 1.0)))
              (p2 (vector (random 1.0) (random 1.0) (random 1.0)))
              (u (vect p0 p1))
              (v (vect p0 p2))
              (uxv (cross-product u v)))

         ;; add in the verticies.
         (setf (attr-ref ds 'position :end) p0
               (attr-ref ds 'position :end) p1
               (attr-ref ds 'position :end) p2)

         ;; add in the normals for each vertex.
         (setf (attr-ref ds 'normal :end) uxv
               (attr-ref ds 'normal :end) uxv
               (attr-ref ds 'normal :end) uxv)

         ;; add in the uv for each vertex
         (setf (attr-ref ds 'uv :end) #(0 0)
               (attr-ref ds 'uv :end) #(1 0)
               (attr-ref ds 'uv :end) #(0 1))))

    (destroy-datastore ds)))

(defun test-3 ()
  (let* ((num-attrs 9)
         (layout-set (test-ds-layout))
         (ds (make-native-datastore 'bvertex layout-set
                                    :size num-attrs
                                    :resizeable-p T)))

    (loop :for i :below (+ num-attrs 5) :do
       (setf (attr-ref ds 'position :end) #(1 1 1))
       (setf (attr-ref ds 'normal :end) #(2 2 2))
       (setf (attr-ref ds 'uv :end) #(1.0 1.0)))


    (inspect ds)

    (destroy-datastore ds)))
