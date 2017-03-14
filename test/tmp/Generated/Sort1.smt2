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
  isort :keep :source Sort.sort
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
  :source Sort.prop_ISortSorts
  (forall ((x (list Int))) (ordered-ordered1 (isort x))))
(assert-not
  :source Sort.prop_ISortCount
  (forall ((x Int) (y (list Int)))
    (= (count x (isort y)) (count x y))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y x)) (= (count y (as nil (list x))) 0))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 Int))
      (= (<= (count y z) x2) (<= 0 x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (+ 1 (count y z)) (count y (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x))
      (= (count z (cons y (as nil (list x))))
        (count y (cons z (as nil (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 Int))
      (= (<= x2 (count y (cons y z))) (<= x2 1)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y Int) (z Int) (x2 x) (x3 (list x)))
      (= (<= (+ y (count x2 x3)) z) (<= y (+ z (count x2 x3)))))))
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
  (forall ((x Int) (y (list Int)))
    (= (<= (insert x y) (as nil (list Int))) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (insert 0 (cons 1 x)) (cons 0 (cons 1 x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (insert 1 (cons 0 x)) (cons 0 (insert 1 x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (insert 1 (insert 0 x)) (insert 0 (insert 1 x)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (insert y (insert x z)) (insert x (insert y z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (<= x (insert y (insert y z))) (<= x (insert y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (<= (insert x (cons x y)) z) (<= (cons x (insert x y)) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (insert x (insert x y)) y) (<= (insert x y) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (cons x (as nil (list Int))) (insert x y))
      (<= (cons x y) (insert x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (insert x (cons (+ x 1) y)) (cons x (cons (+ x 1) y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (insert (+ x 1) (cons x y)) (cons x (insert (+ x 1) y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (<= x (insert 0 (insert 1 x))) (<= x (insert 0 x)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (cons x (as nil (list Int))) (insert 0 y))
      (<= (cons x y) (insert 0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (<= (cons 0 (insert 1 x)) x) (<= (cons 0 x) x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (cons 1 x) (insert 0 y)) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (insert 0 (insert 1 x)) y) (<= (insert 0 (insert 0 x)) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (<= (cons 0 (as nil (list Int))) (insert 1 x))
      (<= (cons 0 x) (insert 0 x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (insert 0 (cons (+ 1 1) x)) (cons 0 (cons (+ 1 1) x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (insert (+ 1 1) (cons 0 x)) (cons 0 (insert (+ 1 1) x)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (<= z (insert y (cons x z))) (<= z (insert x (cons y z))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z Int) (x2 (list Int)))
    (= (<= x2 (insert z (cons y x))) (<= x2 (insert y (cons z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z Int) (x2 (list Int)))
    (= (<= (cons y (insert z x)) x2) (<= (cons y (insert z x2)) x2))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)) (x2 (list Int)))
    (= (<= (insert x (cons y x2)) x2) (<= (insert x (cons y z)) x2))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (insert 0 y) (cons 1 y)) (<= (insert x y) (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (<= (insert y z) (cons y z)) (<= (insert y x) (cons y z)))))
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
  (forall ((x (list Int)))
    (= (<= (isort x) (as nil (list Int)))
      (<= x (isort (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (isort x) x) (<= (isort y) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (cons 1 (isort x)) y) (<= (cons 1 x) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (<= (cons y (insert y x)) z) (<= (cons y (isort z)) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (<= (cons x (as nil (list Int))) (insert y z))
      (<= (cons x z) (insert y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (insert x (isort y)) (isort y)) (<= (insert x y) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (isort y) (insert x (isort y))) (<= y (insert x y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x))
      (= (ordered-ordered1 (cons y (as nil (list x))))
        (ordered-ordered1 (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (= (ordered-ordered1 (cons 0 (as nil (list Int))))
      (ordered-ordered1 (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (ordered-ordered1 (cons y (cons y z)))
        (ordered-ordered1 (cons y z))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered-ordered1 (cons 0 (cons 1 x)))
      (ordered-ordered1 (cons 1 x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered-ordered1 (cons 1 (cons 0 x))) false)))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list Int)))
      (= (ordered-ordered1 (isort y))
        (ordered-ordered1 (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered-ordered1 (insert x y)) (<= y (isort y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 0 (isort x)) (count 0 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 1 (isort x)) (count 1 x))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count x (insert 0 y)) (count x (cons 0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count x (insert 1 y)) (count x (cons 1 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count 0 (insert x y)) (count 0 (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count 1 (insert x y)) (count 1 (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 0 (cons 1 x)) (count 0 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 1 (cons 0 x)) (count 1 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z Int))
    (= (<= (count y (isort x)) z) (<= y (+ y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (count x (cons y (isort z))) (count x (insert y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (count x (insert y (isort z))) (count x (insert y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (count (+ x y) (isort z)) (count (+ x y) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered-ordered1 (insert x (cons x y)))
      (ordered-ordered1 (cons x (insert x y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (ordered-ordered1 (insert x (insert y z))) (<= z (isort z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (cons 0 (isort x)) y) (<= (cons 0 x) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered-ordered1 (cons (count 0 x) x))
      (ordered-ordered1 (cons 1 x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered-ordered1 (cons 0 (insert 1 x)))
      (ordered-ordered1 (cons 0 x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (ordered-ordered1 (cons 1 (insert 0 x))) false)))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 Int) (x3 Int))
      (= (<= x2 (+ x3 (count y z))) (<= x2 x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list Int)) (x3 (list Int)))
      (= (<= x2 (cons (count y z) x3)) (<= x2 (cons 0 x3))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list Int)) (x3 (list Int)))
      (= (<= x2 (insert (count y z) x3)) (<= x2 (insert 0 x3))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list Int)))
      (= (<= (cons (count y z) x2) x2) (<= (cons 0 x2) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list Int)))
      (= (<= (insert (count y z) x2) x2) (<= (insert 0 x2) x2)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count x (cons (+ x x) y)) (count x (cons 0 y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 Int) (x3 (list Int)))
      (= (count x2 (cons (count y z) x3)) (count x2 (cons 0 x3))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 Int) (x3 (list Int)))
      (= (count (+ x2 (count y z)) x3) (count x2 (isort x3))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (count x (cons y (insert 0 z))) (count x (cons y (cons 0 z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int))
    (= (count x (cons (+ y x) (as nil (list Int))))
      (count y (cons 0 (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (count 0 (cons x (insert y z))) (count 0 (cons x (cons y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count 0 (cons (+ x x) y)) (count 0 (cons x y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list Int)))
      (= (insert 0 (cons (count y z) x2))
        (cons 0 (cons (count y z) x2))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (insert 0 (cons (count 0 x) y)) (cons 0 (cons (count 0 x) y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Int) (x2 (list Int)))
    (= (count x (cons z (cons y x2))) (count x (cons y (cons z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)) (z (list Int)))
    (= (<= (cons (count 0 z) x) z) (<= (cons (count 0 z) y) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count (+ x x) (cons 0 y)) (count (+ x x) (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)) (z (list Int)))
    (= (count (count 0 x) z) (count (count 0 y) z))))
(check-sat)
