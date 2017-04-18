(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  smallerEq :keep :source Sort.smallerEq
    ((x (list Int)) (y Int)) Bool
    (match x
      (case nil true)
      (case (cons z xs) (and (<= z y) (smallerEq xs y)))))
(define-fun-rec
  ordered :keep :source Sort.ordered
    ((x (list Int))) Bool
    (match x
      (case nil true)
      (case (cons y z)
        (match z
          (case nil true)
          (case (cons y2 xs) (and (<= y y2) (ordered (cons y2 xs))))))))
(define-fun-rec
  filterLEq :keep :source Sort.filterLEq
    ((x Int) (y (list Int))) (list Int)
    (match y
      (case nil (as nil (list Int)))
      (case (cons b bs)
        (ite (<= b x) (cons b (filterLEq x bs)) (filterLEq x bs)))))
(define-fun-rec
  filterGT :keep :source Sort.filterGT
    ((x Int) (y (list Int))) (list Int)
    (match y
      (case nil (as nil (list Int)))
      (case (cons b bs)
        (ite (> b x) (cons b (filterGT x bs)) (filterGT x bs)))))
(define-fun-rec
  count :keep :source Sort.count
    ((x Int) (y (list Int))) Int
    (match y
      (case nil 0)
      (case (cons z ys) (ite (= x z) (+ 1 (count x ys)) (count x ys)))))
(define-fun-rec
  bigger :keep :source Sort.bigger
    ((x (list Int)) (y Int)) Bool
    (match x
      (case nil true)
      (case (cons z xs) (and (> z y) (bigger xs y)))))
(define-fun-rec
  ++ :keep :source Sort.++
    ((x (list Int)) (y (list Int))) (list Int)
    (match x
      (case nil y)
      (case (cons b bs) (cons b (++ bs y)))))
(define-fun-rec
  qsort :keep :source Sort.qsort
    ((x (list Int))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons y xs)
        (++ (++ (qsort (filterLEq y xs)) (cons y (as nil (list Int))))
          (qsort (filterGT y xs))))))
(assert-not
  :source Sort.prop_count
  (forall ((x Int) (xs (list Int)))
    (= (count x xs) (count x (qsort xs)))))
(assert-not
  :source Sort.prop_QSortSorts
  (forall ((xs (list Int))) (ordered (qsort xs))))
(assert-not
  :source Sort.prop_FALSE
  (forall ((xs (list Int))) (= (ordered (qsort xs)) (ordered xs))))
(check-sat)
