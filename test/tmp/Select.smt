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
(check-sat)
