(in-package :lazy-seq)

(defun nonnegative-integers () (iterate #'1+ 0))
(defun positive-integers    () (tail (nonnegative-integers)))
(defun nonpositive-integers () (iterate #'1- 0))
(defun negative-integers    () (tail (nonpositive-integers)))
(defun alternating-integers () (seq* 0 (interleave (positive-integers) (negative-integers))))
(defun squares              () (seq-fmap (lambda (x) (* x x)) (nonnegative-integers)))
(defun cubes                () (seq-fmap (lambda (x) (* x x x)) (nonnegative-integers)))
