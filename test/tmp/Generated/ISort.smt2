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
  isort :keep :source ISort.sort
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
(assert-not
  :source ISort.prop_ISortSorts
  (forall ((xs (list Int))) (ordered (isort xs))))
(assert-not
  :source ISort.prop_ISortCount
  (forall ((x Int) (xs (list Int)))
    (= (count x (isort xs)) (count x xs))))
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
  (forall ((x Int) (y Int))
    (= (count y (cons x (as nil (list Int))))
      (count x (cons y (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (<= y (count y (cons y x))) (<= y 1))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z Int) (x2 Int))
    (= (<= z (count (+ x2 x) y)) (<= (+ x2 z) x2))))
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
  (forall ((x Int) (y (list Int)))
    (= (count (+ x (count x y)) y) 0)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int))
    (= (count x (cons (+ y x) (as nil (list Int))))
      (count y (cons 0 (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count 0 (cons (+ x x) y)) (count 0 (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count (+ x x) (cons 0 y)) (count (+ x x) (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (insert x (as nil (list Int))) (cons x (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (insert x (cons x y)) (cons x (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (insert y (insert x z)) (insert x (insert y z)))))
(assert-not
  :speculated-lemma
  (= (isort (as nil (list Int))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (isort (isort x)) (isort x))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (isort (cons x y)) (insert x (isort y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (isort (insert x y)) (insert x (isort y)))))
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
    (= (ordered (isort x)) (ordered (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count x (isort y)) (count x y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (insert x y)) (ordered y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (cons x (insert x y))) (ordered (cons x (cons x y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Int) (x2 (list Int)))
    (= (count x (cons y (insert z x2)))
      (count x (cons y (cons z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count (count x (cons 0 y)) y) (count (count x y) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (insert 0 (cons (count x y) z)) (cons 0 (cons (count x y) z)))))
(check-sat)
