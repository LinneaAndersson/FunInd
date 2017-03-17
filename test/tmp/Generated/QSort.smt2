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
  (par (a)
    (filter :let :source Prelude.filter
       ((p (=> a Bool)) (x (list a))) (list a)
       (match x
         (case nil (as nil (list a)))
         (case (cons y xs)
           (ite (@ p y) (cons y (filter p xs)) (filter p xs)))))))
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
        (++ (qsort (filter (lambda ((z Int)) (<= z y)) xs))
          (++ (cons y (as nil (list Int)))
            (qsort (filter (lambda ((x2 Int)) (> x2 y)) xs)))))))
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
  (forall ((x Int) (y (list Int)))
    (= (> x (count 1 y)) (> x (count x y)))))
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
  (forall ((x Int) (y Int) (z Int) (x2 (list Int)))
    (= (<= (+ z x) (+ z y)) (<= x (+ y (count z x2))))))
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
  (par (x)
    (forall ((y (=> x Bool)))
      (= (filter y (as nil (list x))) (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z (list x)))
      (= (filter y (filter y z)) (filter y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z (=> x Bool)) (x2 (list x)))
      (= (filter z (filter y x2)) (filter y (filter z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z x) (x2 (list x)))
      (= (filter y (cons z (filter y x2))) (filter y (cons z x2))))))
(assert-not
  :speculated-lemma
  (= (qsort (as nil (list Int))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (qsort (qsort x)) (qsort x))))
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
  (forall ((x (list Int)) (y (list Int)))
    (= (qsort (++ x (qsort y))) (qsort (++ x y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z (list x)) (x2 (list x)))
      (= (++ (filter y z) (filter y x2)) (filter y (++ z x2))))))
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
    (= (ordered (++ (qsort y) (cons x y)))
      (ordered (++ y (cons x y))))))
(check-sat)
