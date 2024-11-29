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
   #:repeat
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
   #:interleave
   #:seq-length
   #:seq-reverse
   #:head
   #:tail
   #:take
   #:drop
   #:map
   #:ormap
   #:andmap
   #:fold
   #:fmap
   #:pure
   #:fapply
   #:flatmap
   #:make-seq-context)

  ;; constants
  (:export
   #:nonnegative-integers
   #:positive-integers
   #:nonpositive-integers
   #:negative-integers
   #:alternating-integers
   #:squares
   #:cubes))
