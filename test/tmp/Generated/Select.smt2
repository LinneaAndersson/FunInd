(declare-datatypes (a b)
  ((pair :source |Prelude.(,)|
     (pair2 :source |Prelude.(,)| (proj1-pair a) (proj2-pair b)))))
(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  select :let
    ((x Int) (y (list (pair Int (list Int)))))
    (list (pair Int (list Int)))
    (match y
      (case nil (as nil (list (pair Int (list Int)))))
      (case (cons z x2)
        (match z
          (case (pair2 y2 ys)
            (cons (pair2 y2 (cons x ys)) (select x x2)))))))
(define-fun-rec
  select2 :keep :source Select.select
    ((x (list Int))) (list (pair Int (list Int)))
    (match x
      (case nil (as nil (list (pair Int (list Int)))))
      (case (cons y xs) (cons (pair2 y xs) (select y (select2 xs))))))
(define-fun-rec
  (par (a b)
    (map :let :source Prelude.map
       ((f (=> a b)) (x (list a))) (list b)
       (match x
         (case nil (as nil (list b)))
         (case (cons y xs) (cons (@ f y) (map f xs)))))))
(define-fun-rec
  formula2 :let
    ((x (list (pair Int (list Int))))) (list (list Int))
    (match x
      (case nil (as nil (list (list Int))))
      (case (cons y z)
        (match y (case (pair2 y2 ys) (cons (cons y2 ys) (formula2 z)))))))
(define-fun-rec
  formula :let
    ((x (list (pair Int (list Int))))) (list (list Int))
    (match x
      (case nil (as nil (list (list Int))))
      (case (cons y z)
        (match y (case (pair2 y2 ys) (cons (cons y2 ys) (formula z)))))))
(define-fun-rec
  (par (a)
    (elem :let :source Prelude.elem
       ((x a) (y (list a))) Bool
       (match y
         (case nil false)
         (case (cons z xs) (or (= z x) (elem x xs)))))))
(define-fun-rec
  (par (a)
    (deleteBy :source Data.List.deleteBy
       ((x (=> a (=> a Bool))) (y a) (z (list a))) (list a)
       (match z
         (case nil (as nil (list a)))
         (case (cons y2 ys)
           (ite (@ (@ x y) y2) ys (cons y2 (deleteBy x y ys))))))))
(define-fun-rec
  isPermutation :keep :source Select.isPermutation
    ((x (list Int)) (y (list Int))) Bool
    (match x
      (case nil
        (match y
          (case nil true)
          (case (cons z x2) false)))
      (case (cons x3 xs)
        (and (elem x3 y)
          (isPermutation xs
            (deleteBy (lambda ((x4 Int)) (lambda ((x5 Int)) (= x4 x5)))
              x3 y))))))
(define-fun-rec
  count :keep :source Select.count
    ((x Int) (y (list Int))) Int
    (match y
      (case nil 0)
      (case (cons z ys) (ite (= x z) (+ 1 (count x ys)) (count x ys)))))
(define-fun-rec
  (par (a)
    (all :let :source Prelude.all
       ((p (=> a Bool)) (x (list a))) Bool
       (match x
         (case nil true)
         (case (cons y xs) (and (@ p y) (all p xs)))))))
(assert-not
  :source Select.prop_Select
  (forall ((xs (list Int)))
    (=
      (map
        (lambda ((x (pair Int (list Int)))) (match x (case (pair2 y z) y)))
        (select2 xs))
      xs)))
(assert-not
  :source Select.prop_SelectPermutations
  (forall ((xs (list Int)))
    (all (lambda ((x (list Int))) (isPermutation x xs))
      (formula (select2 xs)))))
(assert-not
  :source |Select.prop_SelectPermutations'|
  (forall ((xs (list Int)) (z Int))
    (all (lambda ((x (list Int))) (= (count z xs) (count z x)))
      (formula2 (select2 xs)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool))) (= (all y (as nil (list x))) true))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z x))
      (= (all y (cons z (as nil (list x)))) (@ y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z x) (x2 (list x)))
      (= (all y (cons z (cons z x2))) (all y (cons z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z x) (x2 x) (x3 (list x)))
      (= (all y (cons x2 (cons z x3))) (all y (cons z (cons x2 x3)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int)) (= (count x (as nil (list Int))) 0)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (+ 1 (count x y)) (count x (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 0 (cons 1 x)) (count 0 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (count 1 (cons 0 x)) (count 1 x))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int))
    (= (count y (cons x (as nil (list Int))))
      (count x (cons y (as nil (list Int)))))))
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
  (forall ((x (list Int)) (y Int) (z Int) (x2 (list Int)))
    (= (count (+ y (count z x)) x2) (count (+ y (count z x2)) x2))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x))
      (= (deleteBy y z (as nil (list x))) (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y x)) (= (elem y (as nil (list x))) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x))) (= (elem y (cons y z)) true))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x))
      (= (elem z (cons y (as nil (list x))))
        (elem y (cons z (as nil (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (elem z (cons y (cons z x2))) true))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (elem y (cons z (cons z x2))) (elem y (cons z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem x (cons (+ x x) y)) (elem x (cons 0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int))
    (= (elem x (cons (+ y x) (as nil (list Int))))
      (elem y (cons 0 (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem 0 (cons (+ x x) y)) (elem 0 (cons x (cons x y))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 x) (x3 (list x)))
      (= (elem y (cons x2 (cons z x3)))
        (elem y (cons z (cons x2 x3)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem (+ x x) (cons 0 y)) (elem (+ x x) (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (isPermutation x x) true)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (isPermutation (as nil (list Int)) x)
      (isPermutation x (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (isPermutation y x) (isPermutation x y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (isPermutation y (cons x y)) false)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (isPermutation (as nil (list Int)) (cons x y)) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (isPermutation x (cons y (cons y z))) false)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (isPermutation z (cons x (cons y z))) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z Int) (x2 (list Int)))
    (= (isPermutation x (cons y (cons z x2))) false)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (isPermutation (cons x y) (cons x (as nil (list Int))))
      (isPermutation y (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (isPermutation (cons y x) (cons 0 x))
      (elem y (cons 0 (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z Int))
    (= (isPermutation (cons x (as nil (list Int))) (cons z y))
      (isPermutation (cons x y) (cons z (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (=
      (isPermutation (cons x (as nil (list Int)))
        (cons y (as nil (list Int))))
      (isPermutation (cons x z) (cons y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z Int) (x2 (list Int)))
    (= (isPermutation (cons x y) (cons x x2))
      (isPermutation (cons z y) (cons z x2)))))
(assert-not
  :speculated-lemma
  (= (formula (as nil (list (pair Int (list Int)))))
    (as nil (list (list Int)))))
(assert-not
  :speculated-lemma
  (= (formula2 (as nil (list (pair Int (list Int)))))
    (as nil (list (list Int)))))
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
  (forall ((x Int))
    (= (select x (as nil (list (pair Int (list Int)))))
      (as nil (list (pair Int (list Int)))))))
(assert-not
  :speculated-lemma
  (= (select2 (as nil (list Int)))
    (as nil (list (pair Int (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list (pair Int (list Int)))))
    (= (formula2 x) (formula x))))
(assert-not
  :speculated-lemma
  (forall ((x (list (pair Int (list Int)))))
    (= (elem (as nil (list Int)) (formula x)) false)))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list (pair x x))))
      (= (elem (pair2 z y) x2) (elem (pair2 y z) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list (pair x x))))
      (= (elem (pair2 z z) x2) (elem (pair2 y z) x2)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (elem y (formula (select2 x))) (elem x (formula (select2 y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list (pair Int (list Int)))))
    (= (elem (cons x y) (formula z)) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list (pair Int (list Int)))) (y (list Int)))
    (= (elem (as nil (list Int)) (cons y (formula x)))
      (isPermutation y (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 (list x)))
      (= (all (@ y z) (deleteBy y z x2)) (all (@ y z) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list (list x))))
      (= (elem (cons y z) (cons z x2)) (elem (cons y z) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)) (x3 (list (list x))))
      (= (elem (cons y (cons z x2)) x3) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)) (z x) (x2 (list x)) (x3 (list (list x))))
      (= (elem (cons z (map y x2)) x3) (elem (cons z x2) x3)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (isPermutation (cons x y) (cons x z)) (isPermutation y z))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list (list x))))
      (= (elem (as nil (list x)) (cons (cons y z) x2))
        (elem (as nil (list x)) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)) (z (list x)) (x2 (list (list x))))
      (= (elem (as nil (list x)) (cons (map y z) x2))
        (elem (as nil (list x)) (cons z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list (list x))))
      (= (elem (cons y z) (cons (as nil (list x)) x2))
        (elem (cons y z) x2)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (elem x (cons (cons 0 y) (as nil (list (list Int)))))
      (isPermutation x (cons 0 y)))))
(check-sat)
