(in-package :lazy-seq)


(defmacro define-constant (name value &optional doc)
    `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when doc (list doc))))

(deftype seq-data ()
  `(or null cons lazy-value seq))

;; The type `SEQ' provides the outer interaction layer
;; to hide the implementation details of lazy sequences
;; from the user.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defstruct seq (data nil :type seq-data)))


(define-constant empty-seq (make-seq)
  "An empty `SEQ'")

(defstruct none)
(defstruct just value)
(deftype optional ()
  '(or just none))


(defstruct lazy-value
  "The role of `LAZY-VALUE' is to disambiguate a thunk produced
as part of the lazy sequence functionality from a thunk that
is actually an element of the lazy sequence"
  thunk
  (result (make-none) :type optional))

(defmethod print-object ((obj seq) stream)
  (if *print-readably* (error "~s cannot be printed readably" (type-of obj))
      (format stream "#<~s>" (type-of obj))))

(defmacro Zzz (expr)
  `(make-lazy-value
    :thunk (lambda () ,expr)))

(defmacro seq-cons (head-expr tail-expr)
  `(make-seq :data (Zzz (cons (Zzz ,head-expr) (Zzz ,tail-expr)))))

(defmacro lazy (seq-expr)
  `(make-seq :data (Zzz ,seq-expr)))

(defmacro seq (&rest exprs)
  (if (null exprs)
      `(make-seq :data nil)
      `(seq-cons ,(car exprs)
                  (seq ,@(cdr exprs)))))

(defmacro seq* (expr &rest exprs)
  (if (null exprs) `(lazy ,expr)
      `(seq-append (seq ,expr ,@(butlast exprs))
                   ,(car (last exprs)))))

(defun pull-value (value)
  (if (lazy-value-p value)
      (with-slots (thunk result) value
        (when (none-p result)
          (setf (lazy-value-result value)
                (make-just :value (pull-value (funcall thunk)))))
        (just-value result))
      value))

(defun pull-seq (seq)
  (declare (type seq seq))
  (with-slots (data) seq
    (etypecase data
      (lazy-value
       (setf (seq-data seq) (pull-value data))
       (pull-seq seq))
      (seq
       (setf (seq-data seq) (seq-data data))
       (pull-seq seq))
      (t seq))))

(defun empty-p (seq)
  "Return `T' if the input sequence is empty.  Otherwise, return `NIL'."
  (etypecase seq
    (seq (seq-empty-p seq))
    (cons nil)
    (null t)))

(defun seq-empty-p (seq)
  (let ((seq (pull-seq seq)))
    (with-slots (data) seq
      (etypecase data
        (cons nil)
        (null t)))))


(defun head (seq)
  "Return the first element of the inut sequence.

This function should not be called with an empty
sequence and doing so will result in an error."
  (etypecase seq
    (seq (seq-head seq))
    (cons
     (when (lazy-value-p (car seq))
         (setf (car seq) (pull-value (car seq))))
     (car seq))
    (null (head-of-empty-sequence-error))))

(defun seq-head (seq)
  (let ((seq (pull-seq seq)))
    (with-slots (data) seq
      (etypecase data
        (cons
         (when (lazy-value-p (car data))
           (setf (car data) (pull-value (car data))))
         (car data))
        (null (head-of-empty-sequence-error))))))

(defun lazy-seq-head (seq)
  (let ((seq (pull-seq seq)))
    (with-slots (data) seq
      (etypecase data
        (cons
         ;; Here, the value is not being pulled as it is in seq-head
         ;; to allow structual operations without reifying the
         ;; values
         (car data))
        (null (head-of-empty-sequence-error))))))


(defun head-of-empty-sequence-error ()
  (error "Cannot return the head of an empty-sequence"))


(defun tail (seq)
  "Return the tail of the input sequence, which
may be either a `SEQ' or a `CONS'"
  (etypecase seq
    (seq (seq-tail seq))
    (cons
     (when (lazy-value-p (cdr seq))
       (setf (cdr seq) (pull-value (cdr seq))))
     (cdr seq))
    (null nil)))

(defun seq-tail (seq)
  "Return the tail of the input `SEQ'."
  (let ((seq (pull-seq seq)))
    (with-slots (data) seq
      (etypecase data
        (cons
         (when (lazy-value-p (cdr data))
           (setf (cdr data) (pull-value (cdr data))))
         (cdr data))
        (null empty-seq)))))

(defun ref (seq index)
  (if (> index 0)
      (ref (tail seq) (1- index))
      (head seq)))


(defun iterate-aux (fun init)
  (seq-cons init (iterate-aux fun (funcall fun init))))

(defmacro iterate (fun init)
  (let ((f (gensym "F"))
        (x (gensym "X")))
    `(make-seq
      :data (lazy (let ((,f ,fun)
                       (,x ,init))
                   (seq-cons ,x  (iterate-aux ,f (funcall ,f ,x))))))))

(defun append-aux (seq0 seq1)
 (lazy (if (empty-p seq0) seq1
           (seq-cons (head seq0) (append-aux (tail seq0) seq1)))))

(defmacro seq-append (seq0-expr seq1-expr)
  `(append-aux (lazy ,seq0-expr) (lazy ,seq1-expr)))

(defun seq-length (seq)
  (labels ((recur (seq accum)
             (etypecase seq
               (seq (with-slots (data) seq
                      (etypecase data
                        (cons (recur (cdr data) (1+ accum)))
                        (lazy-value
                         (with-slots (thunk) data
                           (setf data (funcall thunk))
                           (recur seq accum)))
                        (seq
                         (setf data (seq-data data))
                         (recur seq accum))
                        (null accum))))
               (cons (recur (cdr seq) (1+ accum)))
               (null accum)
               (lazy-value (with-slots (thunk) seq (recur (funcall thunk) accum))))))
    (recur seq 0)))


(defun seq-to-list (seq)
  (etypecase seq
    (seq (seq-seq-to-list seq))
    (cons seq)
    (null nil)))

(defun seq-seq-to-list (seq)
  (labels ((recur (seq accum)
             (let ((seq (pull-seq seq)))
               (with-slots (data) seq
                 (etypecase data
                     (cons
                      (when (lazy-value-p (car data))
                        (setf (car data) (pull-value (car data))))
                      (when (lazy-value-p (cdr data))
                        (setf (cdr data) (pull-value (cdr data))))
                      (recur (cdr data)
                             (cons (car data) accum)))
                   (null (reverse accum)))))))
    (recur seq nil)))

(defun seq-reverse (seq)
  (etypecase seq
    (seq (seq-seq-reverse seq))
    (cons (reverse seq))
    (null nil)))

(defun seq-seq-rappend (seq0 seq1)
  (if (empty-p seq0) seq1
      (lazy (seq-seq-rappend (tail seq0) (seq-cons (lazy-seq-head seq0) seq1)))))

(defun seq-seq-reverse (seq)
  (lazy (seq-seq-rappend seq empty-seq)))


(defun drop (seq n)
  (if (<= n 0) seq
      (lazy
        (if (seq-empty-p seq)
            empty-seq
            (drop (tail seq) (1- n))))))


(defun take (seq n)
  (lazy
    (if (seq-empty-p seq) empty-seq
        (if (<= n 0) empty-seq
            (seq-cons (head seq)
                       (take (tail seq) (1- n)))))))

(defun fmap (f seq)
  (etypecase seq
    (seq (seq-fmap f seq))
    (cons (mapcar f seq))
    (null nil)))

(defun seq-fmap (f seq)
  (lazy (if (empty-p seq) empty-seq
            (seq-cons (funcall f (head seq))
                      (seq-fmap f (tail seq))))))

(defun fapply (fun-seq arg-seq)
  (lazy
    (if (empty-p fun-seq) empty-seq
        (seq-append (fmap (head fun-seq) arg-seq)
                    (fapply (tail fun-seq) arg-seq)))))

(defun flatmap (fun seq)
  (lazy
    (if (empty-p seq) empty-seq
        (seq-append (funcall fun (head seq))
                    (flatmap fun (tail seq))))))

(defun pure (x)
  (seq x))

(define-constant seq-ctx
    (make-instance 'monad-operators
      :fmap    #'fmap
      :pure    #'pure
      :fapply  #'fapply
      :flatmap #'flatmap))
