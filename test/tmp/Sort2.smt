(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  (par (a)
    (ordered-ordered1 :let :keep :source SortUtils.ordered
       ((x (list a))) Bool
       (match x
         (case nil true)
         (case (cons y z)
           (match z
             (case nil true)
             (case (cons y2 xs)
               (and (<= y y2) (ordered-ordered1 (cons y2 xs))))))))))
(define-fun-rec
  insert :keep :source Sort.insert
    ((x Int) (y (list Int))) (list Int)
    (match y
      (case nil (cons x (as nil (list Int))))
      (case (cons z xs)
        (ite (<= x z) (cons x (cons z xs)) (cons z (insert x xs))))))
(define-fun-rec
  isort :keep :source Sort.sort :source Sort.isort
    ((x (list Int))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons y xs) (insert y (isort xs)))))
(define-fun-rec
  (par (a)
    (count :keep :source SortUtils.count
       ((x a) (y (list a))) Int
       (match y
         (case nil 0)
         (case (cons z ys)
           (ite (= x z) (+ 1 (count x ys)) (count x ys)))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (insert 1 (insert 0 x)) (insert 0 (insert 1 x)))))
(check-sat)
