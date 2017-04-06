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
  :source Sort.prop_ordering2
  (forall ((xs (list Int)) (ys (list Int)) (z Int))
    (=> (and (smallerEq xs z) (bigger ys z))
      (= (ordered (++ (++ xs (cons z (as nil (list Int)))) ys))
        (and (ordered xs) (ordered ys))))))
(assert-not
  :source Sort.prop_pp
  (forall ((a (list Int)) (b (list Int)) (c (list Int)))
    (= (++ a (++ b c)) (++ (++ a b) c))))
(assert-not
  :source Sort.prop_countcount
  (forall ((x Int) (bs (list Int)) (cs (list Int)))
    (= (+ (count x bs) (count x cs)) (count x (++ bs cs)))))
(assert-not
  :source Sort.p_tmpNil
  (forall ((y Int))
    (= (count y (as nil (list Int)))
      (count y (qsort (as nil (list Int)))))))
(assert-not
  :source Sort.p_tmp
  (forall ((y Int) (x Int) (xs (list Int)))
    (= (count y (cons x xs))
      (+
        (+ (count y (filterLEq x xs))
          (count y (cons x (as nil (list Int)))))
        (count y (filterGT x xs))))))
(assert-not
  :source Sort.prop_QSortCount
  (forall ((x Int) (xs (list Int)))
    (= (count x (qsort xs)) (count x xs))))
(assert-not
  :source Sort.prop_bigpp
  (forall ((a (list Int)) (b (list Int)) (x Int))
    (= (bigger (++ a b) x) (and (bigger a x) (bigger b x)))))
(assert-not
  :source Sort.prop_smallpp1
  (forall ((a (list Int)) (b (list Int)) (x Int))
    (= (smallerEq (++ a b) x) (and (smallerEq a x) (smallerEq b x)))))
(assert-not
  :source Sort.prop1
  (forall ((ys (list Int)) (x Int))
    (= (smallerEq ys x) (= (filterGT x ys) (as nil (list Int))))))
(assert-not
  :source Sort.prop2
  (forall ((ys (list Int)) (x Int))
    (= (bigger ys x) (= (filterLEq x ys) (as nil (list Int))))))
(assert-not
  :source Sort.prop_ssmsms
  (forall ((y Int) (ys (list Int)) (x Int))
    (= (smallerEq (cons y ys) x)
      (smallerEq
        (++ (++ (filterLEq y ys) (cons y (as nil (list Int))))
          (filterGT y ys))
        x))))
(assert-not
  :source Sort.prop_bbmbmb
  (forall ((y Int) (ys (list Int)) (x Int))
    (= (bigger (cons y ys) x)
      (bigger
        (++ (++ (filterLEq y ys) (cons y (as nil (list Int))))
          (filterGT y ys))
        x))))
(assert-not
  :source Sort.prop_smallQ
  (forall ((xs (list Int)) (x Int))
    (= (smallerEq xs x) (smallerEq (qsort xs) x))))
(assert-not
  :source Sort.prop_bigQ
  (forall ((xs (list Int)) (x Int))
    (= (bigger xs x) (bigger (qsort xs) x))))
(assert-not
  :source Sort.prop_ppOrd
  (forall ((x Int) (xs (list Int)))
    (=> (and (ordered xs) (smallerEq xs x))
      (ordered (++ xs (cons x (as nil (list Int))))))))
(assert-not
  :source Sort.prop_ppOrd1
  (forall ((x Int) (xs (list Int)))
    (=> (ordered (++ xs (cons x (as nil (list Int)))))
      (and (ordered xs) (smallerEq xs x)))))
(assert-not
  :source Sort.prop_ppOrd2
  (forall ((x Int) (xs (list Int)))
    (=> (and (ordered xs) (bigger xs x)) (ordered (cons x xs)))))
(assert-not
  :source Sort.prop_orsAnd
  (forall ((a (list Int)) (b (list Int)))
    (=> (ordered (++ a b)) (and (ordered a) (ordered b)))))
(assert-not
  :source Sort.prop_small
  (forall ((x Int) (xs (list Int))) (smallerEq (filterLEq x xs) x)))
(assert-not
  :source Sort.prop_big
  (forall ((x Int) (xs (list Int))) (bigger (filterGT x xs) x)))
(assert-not
  :source Sort.prop_QSortSorts
  (forall ((xs (list Int))) (ordered (qsort xs))))
(check-sat)
