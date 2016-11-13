(in-package :gloss.vao)

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
