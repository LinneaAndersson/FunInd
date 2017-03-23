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
  (par (a)
    (++ :source Prelude.++
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(define-fun-rec
  qsort :keep :source Sort.qsort
    ((x (list Int))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons y xs)
        (++ (qsort (filterLEq y xs))
          (++ (cons y (as nil (list Int))) (qsort (filterGT y xs)))))))
(assert-not
  :source Sort.prop_small
  (forall ((y Int) (xs (list Int))) (smallerEq (filterLEq y xs) y)))
(assert-not
  :source Sort.prop_bigg
  (forall ((y Int) (xs (list Int))) (bigger (filterGT y xs) y)))
(assert-not
  :source Sort.prop_ordSub
  (forall ((x Int) (xs (list Int)))
    (=> (ordered (cons x xs)) (ordered xs))))
(assert-not
  :source Sort.prop_ppOrd
  (forall ((x Int) (xs (list Int)))
    (= (and (ordered xs) (smallerEq xs x))
      (ordered (++ xs (cons x (as nil (list Int))))))))
(assert-not
  :source Sort.prop_ppOrd2
  (forall ((x Int) (xs (list Int)))
    (=> (and (ordered xs) (bigger xs x)) (ordered (cons x xs)))))
(assert-not
  :source Sort.prop_sqf
  (forall ((x Int) (xs (list Int)))
    (smallerEq (qsort (filterLEq x xs)) x)))
(assert-not
  :source Sort.prop_bqf
  (forall ((x Int) (xs (list Int)))
    (bigger (qsort (filterGT x xs)) x)))
(assert-not
  :source Sort.prop_countSmall
  (forall ((xs (list Int)) (x Int))
    (= (count x (filterLEq x xs)) (count x xs))))
(assert-not
  :source Sort.prop_countBig
  (forall ((xs (list Int)) (x Int)) (= (count x (filterGT x xs)) 0)))
(assert-not
  :source Sort.prop_countcount
  (forall ((x Int) (bs (list Int)) (cs (list Int)))
    (= (+ (count x bs) (count x cs)) (count x (++ bs cs)))))
(assert-not
  :source Sort.prop_p
  (forall ((xs (list Int)) (x Int))
    (= (count x (++ (filterLEq x xs) (filterGT x xs))) (count x xs))))
(assert-not
  :source Sort.prop_p2
  (forall ((xs (list Int)) (x Int))
    (=
      (count x
        (++ (filterLEq x xs)
          (++ (cons x (as nil (list Int))) (filterGT x xs))))
      (count x (cons x xs)))))
(assert-not
  :source Sort.prop_qcons
  (forall ((xs (list Int)) (x Int))
    (=> (smallerEq xs x) (= (qsort (cons x xs)) (cons x (qsort xs))))))
(assert-not
  :source Sort.prop_smallSort
  (forall ((x Int) (xs (list Int)))
    (= (smallerEq xs x) (smallerEq (qsort xs) x))))
(assert-not
  :source Sort.prop_biggSort
  (forall ((x Int) (xs (list Int)))
    (= (bigger xs x) (bigger (qsort xs) x))))
(assert-not
  :source Sort.prop_QSortSorts
  (forall ((xs (list Int))) (ordered (qsort xs))))
(assert-not
  :source Sort.prop_filterOrd
  (forall ((x Int) (xs (list Int)))
    (= (ordered xs) (ordered (++ (filterLEq x xs) (filterGT x xs))))))
(assert-not
  :source Sort.prop_QSortCount
  (forall ((x Int) (xs (list Int)))
    (= (count x (qsort xs)) (count x xs))))
(check-sat)
