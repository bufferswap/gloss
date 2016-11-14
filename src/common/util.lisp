(in-package :gloss.common)

;;; General functions

(defun select-if (filter list &key (transform #'identity))
  (mapcar transform (remove-if (complement filter) list)))
