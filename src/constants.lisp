(in-package :lazy-seq)

(define-constant nonnegative-integers (iterate #'1+ 0))
(define-constant positive-integers    (tail nonnegative-integers))
(define-constant nonpositive-integers (iterate #'1- 0))
(define-constant negative-integers    (tail nonpositive-integers))
(define-constant alternating-integers (seq* 0 (interleave positive-integers negative-integers)))
(define-constant squares              (seq-fmap (lambda (x) (* x x)) nonnegative-integers))
(define-constant cubes                (seq-fmap (lambda (x) (* x x x)) nonnegative-integers))
