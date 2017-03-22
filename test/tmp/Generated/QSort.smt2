(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
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
  :source Sort.prop_QSortSorts
  (forall ((xs (list Int))) (ordered (qsort xs))))
(assert-not
  :source Sort.prop_QSortCount
  (forall ((x Int) (xs (list Int)))
    (= (count x (qsort xs)) (count x xs))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y (list x))) (= (++ y (as nil (list x))) y))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y (list x))) (= (++ (as nil (list x)) y) y))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list x)))
      (= (++ (++ y z) x2) (++ y (++ z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (cons y (++ z x2)) (++ (cons y z) x2)))))
(assert-not
  :speculated-lemma
  (forall ((x Int)) (= (count x (as nil (list Int))) 0)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (+ 1 (count x y)) (count x (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int))) (= (<= 0 (count x y)) true)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int))) (= (<= (count x y) 1) true)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int))) (= (> (count x y) 1) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 0 (cons 1 x)) (count 0 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 1 (cons 0 x)) (count 1 x))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= x (count 1 y)) (<= x (count x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (<= (count 0 x) y) (<= (count y x) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (> x (count 1 y)) (> x (count x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (> (count 0 x) y) (> (count y x) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int))
    (= (count y (cons x (as nil (list Int))))
      (count x (cons y (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z Int))
    (= (<= z (count x (cons x y))) (<= z 1))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (<= (+ x x) (+ y y)) (<= x (+ y (count y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Int) (x2 (list Int)))
    (= (<= (+ x (count z x2)) y) (<= x (+ y (count z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count x (cons (+ x x) y)) (count x (cons 0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int))
    (= (count x (cons (+ y x) (as nil (list Int))))
      (count y (cons 0 (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count x (cons (+ x 1) y)) (count x y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count 0 (cons (+ x x) y)) (count 0 (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count 1 (cons (+ x x) y)) (count 1 y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (count 1 (cons (count x y) z)) (+ (count x y) (count 1 z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (filterGT x (as nil (list Int))) (as nil (list Int)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (filterGT x (cons x y)) (filterGT x y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (filterGT x (filterGT x y)) (filterGT x (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterGT y (filterGT x z)) (filterGT x (filterGT y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterGT x (cons y (cons x z))) (filterGT x (cons y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterGT y (cons x (filterGT y z)))
      (filterGT y (cons x (cons y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterGT x (filterGT y (cons x z)))
      (filterGT x (cons x (filterGT y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (filterGT (+ x x) (cons 0 (as nil (list Int))))
      (filterGT x (cons 0 (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (filterGT 0 (cons (+ x x) (as nil (list Int))))
      (filterGT x (cons (+ x x) (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (filterLEq x (as nil (list Int))) (as nil (list Int)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (filterLEq x (cons x y)) (cons x (filterLEq x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (filterLEq x (filterLEq x y)) (filterLEq x y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterLEq y (filterLEq x z)) (filterLEq x (filterLEq y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterLEq x (cons y (filterLEq x z)))
      (filterLEq x (cons y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterLEq x (filterLEq y (cons y z)))
      (filterLEq x (cons y (filterLEq y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterLEq x (filterLEq y (cons y z)))
      (filterLEq x (cons y (filterLEq y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (filterLEq (+ x x) (cons 0 (as nil (list Int))))
      (filterLEq x (cons 0 (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterLEq y (filterLEq x (cons y z)))
      (filterLEq x (cons y (filterLEq y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (filterLEq 0 (cons (+ x x) (as nil (list Int))))
      (filterLEq x (cons (+ x x) (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (= (qsort (as nil (list Int))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (qsort (qsort x)) (qsort x))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (qsort (filterGT x y)) (filterGT x (qsort y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (qsort (filterLEq x y)) (filterLEq x (qsort y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (qsort (cons x (as nil (list Int))))
      (cons x (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (qsort (++ y x)) (qsort (++ x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (filterGT x (filterLEq x y)) (as nil (list Int)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (filterLEq y (filterGT x z)) (filterGT x (filterLEq y z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (qsort (++ x (qsort y))) (qsort (++ x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (qsort (cons x (filterGT x y)))
      (cons x (filterGT x (qsort y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (filterGT x (filterGT 0 (qsort y)))
      (filterGT x (filterGT 0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (++ (filterGT x y) (filterGT x z)) (filterGT x (++ y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (++ (filterLEq x y) (filterLEq x z)) (filterLEq x (++ y z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (filterLEq y (++ z (filterGT y x))) (filterLEq y z))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (filterLEq y (++ (filterGT y x) z)) (filterLEq y z))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (ordered (cons x (as nil (list Int))))
      (ordered (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (cons x (cons x y))) (ordered (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered (qsort x)) (ordered (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered (++ x (qsort x))) (ordered (++ x x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered (++ (qsort x) x)) (ordered (++ x x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered (++ x (++ x x))) (ordered (++ x x)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (cons x (filterGT x y))) (ordered (filterGT x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (cons x (filterLEq x y)))
      (ordered (cons x (qsort y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (ordered (filterGT y (cons x z)))
      (ordered (cons x (filterGT y z))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered (++ (qsort x) (qsort x))) (ordered (++ x x)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (++ (cons x y) (qsort y)))
      (ordered (++ (cons x y) y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (++ (filterLEq x y) (qsort y)))
      (ordered (filterLEq x (++ y y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (++ (qsort y) (cons x y)))
      (ordered (++ y (cons x y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (++ (qsort y) (filterGT x y)))
      (ordered (filterGT x (++ y y))))))
(check-sat)
