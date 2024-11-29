(defpackage :lazy-seq-test
  (:use :cl :5am :lazy-seq)
  (:shadowing-import-from :lazy-seq :map)
  (:export :run-all-tests!))

(in-package :lazy-seq-test)


(def-suite lazy-seq)

(in-suite lazy-seq)

(test iterate
  (let ((xs (iterate #'1+ 0)))
    (is (seq-p xs))
    (is (not (empty-p xs)))
    (is (= 0 (head xs)))
    (is (= 1 (head (tail xs))))
    (is (= 2 (head (tail (tail xs)))))))

(test emtpy-seq
    (is (seq-p empty-seq))
    (is (empty-p empty-seq)))

(test seq-cons
    (let ((xs (seq-cons 'x empty-seq)))
      (is (seq-p xs))
      (is (not (empty-p (seq-cons 'x empty-seq))))
      (is (eq 'x (head (seq-cons 'x empty-seq))))))

(test seq
  (let ((counter 0))
    (labels ((inc () (setf counter (1+ counter)))
             (a () (inc) 'a)
             (b () (inc) 'b)
             (c () (inc) 'c))
      (let ((xs (seq (a) (b) (c))))
        (is (seq-p xs))
        (is (zerop counter))

        (is (eq 'a (head xs)))
        (is (= 1 counter))

        (is (eq 'b (head (tail xs))))
        (is (= 2 counter))

        (is (eq 'c (head (tail (tail xs)))))
        (is (= 3 counter))))))

(test seq*-with-only-final-arg
  (let ((seq (seq* (seq 1 2 3))))
    (is (seq-p seq))
    (is (= 3 (seq-length seq)))
    (is (equal (list 1 2 3)
               (seq-to-list seq)))))

(test seq*-with-additional-args
  (let* ((seq0 (seq 4 5 6))
         (seq1 (seq* 1 2 3 seq0)))
    (is (seq-p seq1))
    (is (= 6 (seq-length seq1)))
    (is (equal '(1 2 3 4 5 6) (seq-to-list seq1)))))

(test seq-append
  (let ((counter 0))
    (labels ((inc () (setf counter (1+ counter)))
             (a () (inc) 'a)
             (b () (inc) 'b)
             (c () (inc) 'c)
             (d () (inc) 'd)
             (e () (inc) 'e)
             (f () (inc) 'f))
      (let* ((seq0 (seq (a) (b) (c)))
             (seq1 (seq (d) (e) (f)))
             (seq2 (seq-append seq0 seq1)))
        (is (= 6 (seq-length seq2)))
        (is (= 0 counter))
        (is (seq-p seq2))
        (is (not (empty-p seq2)))
        (is (= 0 counter))

        ;; returning the length only reifies the structure, not
        ;; the values.
        (is (= 6 (seq-length seq2)))
        (is (= 0 counter))

        ;; not here that the counter is only 1, indicating
        ;; that the first value was not reified in the process
        ;; of accessing the second value.
        (is (eq 'b (ref seq2 1)))
        (is (= 1 counter))

        ;; accessing the second value in seq0 did not need to
        ;; reify the second value because it was already reified
        ;; while accessing it from seq2
        (is (eq 'b (ref seq0 1)))
        (is (= 1 counter))

        ;; just like above, the last value was accessed without
        ;; reification of the previous values in the sequence
        (is (eq 'f (ref seq2 5)))
        (is (= 2 counter))

        ;; accessing the an element from seq1 does not reify a
        ;; value because it was already reified when accessing
        ;; the value from seq2
        (is (eq 'f (ref seq1 2)))
        (is (= 2 counter))))))

(test seq-reverse
  (let* ((seq (seq 1 2 3))
         (rseq (seq-reverse seq)))
    (is (equal (reverse (seq-to-list seq))
               (seq-to-list rseq)))))

(test take
  (let* ((seq0 (seq 1 2 3 4 5))
         (seq1 (take seq0 2)))
    (is (seq-p seq1))
    (is (= 2 (seq-length seq1)))
    (is (equal '(1 2) (seq-to-list seq1)))))

(test drop
  (let* ((seq0 (seq 1 2 3 4 5))
         (seq1 (drop seq0 3)))
    (is (seq-p seq1))
    (is (= 2 (seq-length seq1)))
    (is (equal '(4 5) (seq-to-list seq1)))))

(test fmap
  (let* ((seq0 (seq 'a 'b 'c))
         (seq1 (fmap #'symbol-name seq0)))
    (is (seq-p seq1))
    (is (equal '("A" "B" "C") (seq-to-list seq1)))))

(test fapply
  (let* ((fun-seq (seq #'symbol-name))
         (arg-seq (seq 'a 'b 'c))
         (seq (fapply fun-seq arg-seq)))
    (is (seq-p seq))
    (is (equal '("A" "B" "C") (seq-to-list seq)))))


(test flatmap
  (flet ((fun (x) (seq x x)))
    (let* ((seq0 (seq 'a 'b))
           (seq1 (flatmap #'fun seq0)))
      (is (seq-p seq1))
      (is (equal '(a a b b)
                 (seq-to-list seq1))))))

(test constants
  (is (= 0 (ref nonnegative-integers 0)))
  (is (= 1 (ref nonnegative-integers 1)))

  (is (= 1 (ref positive-integers 0)))
  (is (= 2 (ref positive-integers 1)))

  (is (= 0 (ref nonpositive-integers 0)))
  (is (= -1 (ref nonpositive-integers 1)))

  (is (= -1 (ref negative-integers 0)))
  (is (= -2 (ref negative-integers 1)))

  (is (= 0 (ref squares 0)))
  (is (= 1 (ref squares 1)))
  (is (= 4 (ref squares 2)))

  (is (= 0 (ref cubes 0)))
  (is (= 1 (ref cubes 1)))
  (is (= 8 (ref cubes 2))))