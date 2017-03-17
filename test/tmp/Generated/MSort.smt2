(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  ordered :keep :source MSort.ordered
    ((x (list Int))) Bool
    (match x
      (case nil true)
      (case (cons y z)
        (match z
          (case nil true)
          (case (cons y2 xs) (and (<= y y2) (ordered (cons y2 xs))))))))
(define-fun-rec
  (par (a b)
    (map :let :source Prelude.map
       ((f (=> a b)) (x (list a))) (list b)
       (match x
         (case nil (as nil (list b)))
         (case (cons y xs) (cons (@ f y) (map f xs)))))))
(define-fun-rec
  (par (a)
    (lmerge :keep :source MSort.lmerge
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z x2)
           (match y
             (case nil (cons z x2))
             (case (cons x3 x4)
               (ite
                 (<= z x3) (cons z (lmerge x2 (cons x3 x4)))
                 (cons x3 (lmerge (cons z x2) x4))))))))))
(define-fun-rec
  (par (a)
    (pairwise-pairwise1 :let :keep :source MSort.pairwise
       ((x (list (list a)))) (list (list a))
       (match x
         (case nil (as nil (list (list a))))
         (case (cons xs y)
           (match y
             (case nil (cons xs (as nil (list (list a)))))
             (case (cons ys xss)
               (cons (lmerge xs ys) (pairwise-pairwise1 xss)))))))))
(define-fun-rec
  mergingbu :keep :source MSort.mergingbu
    ((x (list (list Int)))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons xs y)
        (match y
          (case nil xs)
          (case (cons z x2)
            (mergingbu (pairwise-pairwise1 (cons xs (cons z x2)))))))))
(define-fun
  msortbu :keep :source MSort.msortbu
    ((x (list Int))) (list Int)
    (mergingbu
      (map (lambda ((y Int)) (cons y (as nil (list Int)))) x)))
(define-fun-rec
  count :keep :source MSort.count
    ((x Int) (y (list Int))) Int
    (match y
      (case nil 0)
      (case (cons z ys) (ite (= x z) (+ 1 (count x ys)) (count x ys)))))
(assert-not
  :source MSort.prop_MSortBUSorts
  (forall ((xs (list Int))) (ordered (msortbu xs))))
(assert-not
  :source MSort.prop_MSortBUCount
  (forall ((x Int) (xs (list Int)))
    (= (count x (msortbu xs)) (count x xs))))
(assert-not
  :speculated-lemma
  (forall ((x Int)) (= (count x (as nil (list Int))) 0)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z Int))
    (= (<= z (count x y)) (<= z 0))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (+ 1 (count x y)) (count x (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int)) (= (<= (count 1 x) y) (<= 0 y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 0 (cons 1 x)) (count 0 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 1 (cons 0 x)) (count 1 x))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z Int))
    (= (<= (count z y) z) (<= (count x y) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int))
    (= (count y (cons x (as nil (list Int))))
      (count x (cons y (as nil (list Int)))))))
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
    (= (count x (cons (count 1 y) y)) (count x (cons 0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count 0 (cons (+ x x) y)) (count 0 (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count (count x (cons 0 y)) y) (count (count x y) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count (count 0 (cons x y)) y) (count (count 0 y) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (<= (cons (count 0 x) (as nil (list Int))) x)
      (<= (cons 1 (as nil (list Int))) x))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (count x (cons 0 (cons y z))) (count x (cons y (cons 0 z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count (+ x x) (cons 0 y)) (count (+ x x) (cons x y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x))) (= (lmerge y (as nil (list x))) y))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x))) (= (lmerge (as nil (list x)) y) y))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list x)))
      (= (lmerge (lmerge y z) x2) (lmerge y (lmerge z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (<= (lmerge y y) (as nil (list x))) (<= y (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list x)))
      (= (<= y (lmerge x2 z)) (<= y (lmerge z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list x)))
      (= (<= (lmerge z y) x2) (<= (lmerge y z) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list x)))
      (= (<= y (lmerge z (lmerge x2 x2))) (<= y (lmerge z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (<= (cons z (lmerge x2 y)) x2) (<= (cons z x2) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (<= (cons z (lmerge y x2)) x2) (<= (cons z x2) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (<= (cons y (lmerge z z)) x2) (<= (cons y z) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)))
      (= (<= (lmerge y (lmerge y z)) z) (<= (lmerge y z) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)))
      (= (<= (lmerge y (lmerge y y)) z) (<= (lmerge y y) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (lmerge (cons z y) (cons z x2))
        (cons z (lmerge y (cons z x2)))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z Int))
    (= (lmerge (cons (+ y z) (as nil (list Int))) x)
      (lmerge x (cons (+ y z) (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 x) (x3 (list x)))
      (= (<= y (lmerge z (cons x2 y))) (<= y (lmerge z (cons x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list x)) (x3 (list x)))
      (= (<= y (lmerge z (lmerge x3 x2)))
        (<= y (lmerge z (lmerge x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)) (x3 (list x)))
      (= (<= (cons y (lmerge x2 z)) x3)
        (<= (cons y (lmerge z x2)) x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)) (x3 (list x)))
      (= (<= (cons z (lmerge y x2)) x3) (<= (cons z x2) x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)) (x3 (list x)))
      (= (<= (lmerge y (cons z y)) x3) (<= (lmerge y (cons z x2)) x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list x)) (x3 (list x)))
      (= (<= (lmerge y (lmerge x2 z)) x3)
        (<= (lmerge y (lmerge z x2)) x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x))
      (= (lmerge (cons z (as nil (list x))) (cons y (as nil (list x))))
        (lmerge (cons y (as nil (list x))) (cons z (as nil (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (= (pairwise-pairwise1 (as nil (list (list x))))
      (as nil (list (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (pairwise-pairwise1 (cons y (as nil (list (list x)))))
        (cons y (as nil (list (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list (list x))))
      (= (pairwise-pairwise1 (pairwise-pairwise1 (pairwise-pairwise1 y)))
        (pairwise-pairwise1 (pairwise-pairwise1 y))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list (list x))))
      (= (lmerge (cons y z) z) (lmerge z (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x))
      (= (lmerge (cons z (as nil (list x))) y)
        (lmerge y (cons z (as nil (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list (list x))) (z (list (list x))))
      (= (lmerge y (cons (as nil (list x)) z))
        (cons (as nil (list x)) (lmerge y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list (list x))) (z (list (list x))))
      (= (lmerge (pairwise-pairwise1 z) (pairwise-pairwise1 y))
        (lmerge (pairwise-pairwise1 y) (pairwise-pairwise1 z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list (list x))))
      (= (pairwise-pairwise1 (cons y (cons z x2)))
        (cons (lmerge y z) (pairwise-pairwise1 x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list (list x))))
      (= (lmerge (cons y (cons y z)) z)
        (lmerge z (cons y (cons y z)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list (list x))) (x2 (list (list x))))
      (= (lmerge (cons y (lmerge x2 z)) x2)
        (lmerge x2 (cons y (lmerge z x2)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (lmerge (cons (+ x x) y) y) (lmerge y (cons (+ x x) y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (lmerge (cons (+ x y) z) z) (lmerge z (cons (+ x y) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall
      ((y (list (list x))) (z x) (x2 (list x)) (x3 (list (list x))))
      (= (lmerge (cons (cons z x2) x3) y)
        (lmerge y (cons (cons z x2) x3))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list (list x))) (z (list (list x))))
      (= (<= (lmerge y (pairwise-pairwise1 y)) (pairwise-pairwise1 z))
        (<= (lmerge y y) (pairwise-pairwise1 z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list (list x))) (z (list (list x))))
      (= (<= (pairwise-pairwise1 y) (lmerge z (pairwise-pairwise1 z)))
        (<= (pairwise-pairwise1 y) (lmerge z z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 (list (list x))))
      (= (<= (cons (lmerge z y) x2) x2)
        (<= (cons (lmerge y z) x2) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (lmerge (cons z x2) (cons y x2))
        (lmerge (cons y x2) (cons z x2))))))
(assert-not
  :speculated-lemma
  (= (mergingbu (as nil (list (list Int)))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x (list (list Int))))
    (= (mergingbu (pairwise-pairwise1 x)) (mergingbu x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list (list Int))))
    (= (mergingbu (cons x y)) (lmerge x (mergingbu y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list (list x))) (z (list (list x))))
      (= (<= y (pairwise-pairwise1 (pairwise-pairwise1 z)))
        (<= y (pairwise-pairwise1 z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list (list x))) (z (list (list x))))
      (= (<= (pairwise-pairwise1 (pairwise-pairwise1 y)) z)
        (<= (pairwise-pairwise1 y) z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list (list Int))) (y (list (list Int))))
    (= (lmerge (mergingbu y) (mergingbu x)) (mergingbu (lmerge x y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall
      ((y (list (list x))) (z (list (list x))) (x2 (list (list x))))
      (= (lmerge y (lmerge x2 z)) (lmerge y (lmerge z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (lmerge (cons x (cons x y)) y) (lmerge y (cons x (cons x y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (lmerge (cons x (lmerge y y)) y)
      (lmerge y (cons x (lmerge y y))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list (list x))) (z (list (list x))))
      (= (<= (lmerge y (pairwise-pairwise1 z)) (pairwise-pairwise1 z))
        (<= (lmerge y y) (pairwise-pairwise1 z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)))
      (= (map y (as nil (list x))) (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)) (z x))
      (= (map y (cons z (as nil (list x))))
        (cons (@ y z) (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)) (z x) (x2 (list x)))
      (= (cons (@ y z) (map y x2)) (map y (cons z x2))))))
(assert-not
  :speculated-lemma
  (= (msortbu (as nil (list Int))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (msortbu (msortbu x)) (msortbu x))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (msortbu (cons x (as nil (list Int))))
      (cons x (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (msortbu x) x) (<= (msortbu y) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (msortbu (lmerge y x)) (msortbu (lmerge x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (lmerge (msortbu x) (msortbu y)) (msortbu (lmerge x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (msortbu (cons x (msortbu y))) (msortbu (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list (list Int))) (y (list (list Int))))
    (= (lmerge y (lmerge x x)) (lmerge x (lmerge x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (msortbu (lmerge x x)) y) (<= (lmerge x (msortbu x)) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (msortbu (lmerge y (cons x z)))
      (msortbu (cons x (lmerge y z))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (msortbu x) (msortbu (cons 0 x)))
      (<= x (lmerge x (cons 0 y))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (msortbu (cons 0 y)) (msortbu y))
      (<= (lmerge y (cons 0 x)) y))))
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
    (= (ordered (msortbu x)) (ordered (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (<= x (msortbu x)) (ordered x))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count x (msortbu y)) (count x y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (ordered (lmerge x x)) (ordered x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (ordered (lmerge y x)) (ordered (lmerge x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (ordered (lmerge y (msortbu x))) (ordered y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (count x (cons y (msortbu z))) (count x (cons y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (count x (lmerge y (msortbu z))) (count x (lmerge y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (cons x (lmerge y y))) (ordered (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (ordered (lmerge y (cons x y)))
      (ordered (cons x (lmerge y y))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (ordered (lmerge x (lmerge x y))) (ordered (lmerge x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (ordered (cons x (lmerge z y)))
      (ordered (cons x (lmerge y z))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)) (z (list Int)))
    (= (ordered (lmerge x (lmerge z y)))
      (ordered (lmerge x (lmerge y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (+ (count x y) (count x z)) (count x (lmerge y z)))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z (list x)) (x2 (=> y x)) (x3 (list y)))
      (= (<= z (map x2 (lmerge x3 x3))) (<= z (map x2 x3))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count (+ x (count x y)) y) 0)))
(assert-not
  :speculated-lemma
  (forall ((x (list (list Int))) (y (list Int)))
    (= (<= (mergingbu x) (lmerge y (msortbu y)))
      (<= (mergingbu x) (msortbu y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (msortbu (lmerge y x)) (msortbu y))
      (<= (lmerge y (msortbu x)) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list (list Int))))
    (= (lmerge (lmerge (mergingbu y) x) (mergingbu y))
      (mergingbu (lmerge y (cons x y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)) (x2 (list Int)))
    (= (+ y (count (count x z) x2)) (+ y (count (count y z) x2)))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z (list x)) (x2 (=> y x)) (x3 (list y)) (x4 (list y)))
      (= (<= z (map x2 (lmerge x4 x3)))
        (<= z (map x2 (lmerge x3 x4)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Int) (x2 (list Int)))
    (= (count x (cons z (cons y x2))) (count x (cons y (cons z x2))))))
(check-sat)
