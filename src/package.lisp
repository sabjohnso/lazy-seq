(in-package :cl-user)

(defpackage :lazy-seq
  (:nicknames :lz)
  (:use :cl :contextual)
  (:shadow
   #:copy-seq
   #:map)
  (:export
   #:seq
   #:seq*
   #:iterate
   #:build-seq
   #:seq-p
   #:empty-seq
   #:empty-p
   #:ref
   #:seq-to-list
   #:lazy
   #:seq-cons
   #:seq-append
   #:seq-length
   #:seq-reverse
   #:head
   #:tail
   #:take
   #:drop
   ;; #:map
   ;; #:ormap
   ;; #:andmap
   ;; #:fold
   ;; #:infix
   ;; #:prefix
   ;; #:suffix
   ;; #:circumfix
   #:fmap
   #:pure
   ;; #:product
   #:fapply
   #:flatmap
   #:seq-ctx)

  ;; constants
  (:export
   #:nonnegative-integers
   #:positive-integers
   #:nonpositive-integers
   #:negative-integers
   #:alternating-integers
   #:squares
   #:cubes
   ;; #:primes
   ))
