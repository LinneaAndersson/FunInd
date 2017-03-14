(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  ordered :keep :source SortUtils.ordered
    ((x (list Int))) Bool
    (match x
      (case nil true)
      (case (cons y z)
        (match z
          (case nil true)
          (case (cons y2 xs) (and (<= y y2) (ordered (cons y2 xs))))))))
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
  count :keep :source SortUtils.count
    ((x Int) (y (list Int))) Int
    (match y
      (case nil 0)
      (case (cons z ys) (ite (= x z) (+ 1 (count x ys)) (count x ys)))))
(assert-not
  :source Sort.prop_ISortSorts
  (forall ((x (list Int))) (ordered (isort x))))
(assert-not
  :source Sort.prop_ISortCount
  (forall ((x Int) (y (list Int)))
    (= (count x (isort y)) (count x y))))
(check-sat)
