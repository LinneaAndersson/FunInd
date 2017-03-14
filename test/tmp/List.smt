(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  (par (a b)
    (map :let :source Prelude.map
       ((f (=> a b)) (x (list a))) (list b)
       (match x
         (case nil (as nil (list b)))
         (case (cons y xs) (cons (@ f y) (map f xs)))))))
(define-fun-rec
  (par (a)
    (filter :let :source Prelude.filter
       ((p (=> a Bool)) (x (list a))) (list a)
       (match x
         (case nil (as nil (list a)))
         (case (cons y xs)
           (ite (@ p y) (cons y (filter p xs)) (filter p xs)))))))
(define-fun-rec
  (par (a)
    (nubBy :source Data.List.nubBy
       ((x (=> a (=> a Bool))) (y (list a))) (list a)
       (match y
         (case nil (as nil (list a)))
         (case (cons z xs)
           (cons z
             (nubBy x (filter (lambda ((y2 a)) (not (@ (@ x z) y2))) xs))))))))
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
  isPermutation :keep :source List.isPermutation
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
  deleteAll :keep :source List.deleteAll
    ((x Int) (y (list Int))) (list Int)
    (match y
      (case nil (as nil (list Int)))
      (case (cons z ys)
        (ite (= x z) (deleteAll x ys) (cons z (deleteAll x ys))))))
(define-fun-rec
  count :keep :source List.count
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
  (par (a)
    (!! :source Prelude.!!
       ((x (list a)) (y Int)) a
       (match (< y 0)
         (case false
           (match x (case (cons z x2) (ite (= y 0) z (!! x2 (- y 1))))))))))
(assert-not
  :source List.prop_append_inj_1
  (par (a)
    (forall ((xs (list a)) (ys (list a)) (zs (list a)))
      (=> (= (++ xs zs) (++ ys zs)) (= xs ys)))))
(assert-not
  :source List.prop_append_inj_2
  (par (a)
    (forall ((xs (list a)) (ys (list a)) (zs (list a)))
      (=> (= (++ xs ys) (++ xs zs)) (= ys zs)))))
(assert-not
  :source List.prop_nub_nub
  (par (a)
    (forall ((xs (list a)))
      (=
        (nubBy (lambda ((x a)) (lambda ((y a)) (= x y)))
          (nubBy (lambda ((z a)) (lambda ((x2 a)) (= z x2))) xs))
        (nubBy (lambda ((x3 a)) (lambda ((x4 a)) (= x3 x4))) xs)))))
(assert-not
  :source List.prop_elem_nub_l
  (par (a)
    (forall ((x a) (xs (list a)))
      (=> (elem x xs)
        (elem x (nubBy (lambda ((y a)) (lambda ((z a)) (= y z))) xs))))))
(assert-not
  :source List.prop_elem_nub_r
  (par (a)
    (forall ((x a) (xs (list a)))
      (=> (elem x (nubBy (lambda ((y a)) (lambda ((z a)) (= y z))) xs))
        (elem x xs)))))
(assert-not
  :source List.prop_count_nub
  (forall ((x Int) (xs (list Int)))
    (=> (elem x xs)
      (=
        (count x (nubBy (lambda ((y Int)) (lambda ((z Int)) (= y z))) xs))
        1))))
(assert-not
  :source List.prop_perm_trans
  (forall ((xs (list Int)) (ys (list Int)) (zs (list Int)))
    (=> (isPermutation xs ys)
      (=> (isPermutation ys zs) (isPermutation xs zs)))))
(assert-not
  :source List.prop_perm_refl
  (forall ((xs (list Int))) (isPermutation xs xs)))
(assert-not
  :source List.prop_perm_symm
  (forall ((xs (list Int)) (ys (list Int)))
    (=> (isPermutation xs ys) (isPermutation ys xs))))
(assert-not
  :source List.prop_perm_elem
  (forall ((x Int) (xs (list Int)) (ys (list Int)))
    (=> (elem x xs) (=> (isPermutation xs ys) (elem x ys)))))
(assert-not
  :source List.prop_deleteAll_count
  (forall ((x Int) (xs (list Int)))
    (=>
      (= (deleteAll x xs)
        (deleteBy (lambda ((y Int)) (lambda ((z Int)) (= y z))) x xs))
      (<= (count x xs) 1))))
(assert-not
  :source List.prop_elem
  (par (a)
    (forall ((x a) (xs (list a)))
      (=> (elem x xs) (exists ((y Int)) (= x (!! xs y)))))))
(assert-not
  :source List.prop_elem_map
  (par (a)
    (forall ((y a) (f (=> a a)) (xs (list a)))
      (=> (elem y (map f xs))
        (exists ((x a)) (and (= (@ f x) y) (elem y xs)))))))
(check-sat)
