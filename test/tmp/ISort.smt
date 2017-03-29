(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  ordered :keep :source ISort.ordered
    ((x (list Int))) Bool
    (match x
      (case nil true)
      (case (cons y z)
        (match z
          (case nil true)
          (case (cons y2 xs) (and (<= y y2) (ordered (cons y2 xs))))))))
(define-fun-rec
  insert :keep :source ISort.insert
    ((x Int) (y (list Int))) (list Int)
    (match y
      (case nil (cons x (as nil (list Int))))
      (case (cons z xs)
        (ite (<= x z) (cons x (cons z xs)) (cons z (insert x xs))))))
(define-fun-rec
  isort :keep :source ISort.sort :source ISort.isort
    ((x (list Int))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons y xs) (insert y (isort xs)))))
(define-fun-rec
  count :keep :source ISort.count
    ((x Int) (y (list Int))) Int
    (match y
      (case nil 0)
      (case (cons z ys) (ite (= x z) (+ 1 (count x ys)) (count x ys)))))
(define-fun-rec
  (par (a)
    (++ :source Prelude.++
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(assert-not
  :source ISort.prop_countcount
  (forall ((x Int) (bs (list Int)) (cs (list Int)))
    (= (+ (count x bs) (count x cs)) (count x (++ bs cs)))))
(assert-not
  :source ISort.prop_ISortSorts
  (forall ((xs (list Int))) (ordered (isort xs))))
(assert-not
  :source ISort.prop_ISortCount
  (forall ((x Int) (xs (list Int)))
    (= (count x (isort xs)) (count x xs))))
(assert-not
  :source ISort.prop_FALSE
  (forall ((xs (list Int))) (= (ordered (isort xs)) (ordered xs))))
(check-sat)
