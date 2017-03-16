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
(assert-not
  :speculated-lemma
  (par (x) (forall ((y (list x)) (z x)) (= (!! (cons z y) 0) z))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 x))
      (= (!! (cons y (cons x2 z)) 1) x2))))
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
  (forall ((x (list Int)) (y Int) (z Int))
    (= (< (count y x) z) (< y (+ y z)))))
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
  (forall ((x Int) (y Int) (z (list Int)))
    (= (count x (cons (- x y) z)) (count x (cons (+ x y) z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (count (+ x (count x y)) y) 0)))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (deleteAll x (as nil (list Int))) (as nil (list Int)))))
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
  (forall ((x (list Int)))
    (= (isPermutation (as nil (list Int)) x)
      (isPermutation x (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (isPermutation (as nil (list Int)) (as nil (list Int)))
      (isPermutation x x))))
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
  (par (x)
    (forall ((y (=> x (=> x Bool))))
      (= (nubBy y (as nil (list x))) (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)))
      (= (nubBy y (nubBy y z)) (nubBy y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x))
      (= (nubBy y (cons z (as nil (list x))))
        (cons z (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z (=> x (=> x Bool))) (x2 (list x)))
      (= (nubBy z (nubBy y x2)) (nubBy y (nubBy z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (=> x Bool)) (x2 (list x)))
      (= (filter z (nubBy y (filter z x2))) (nubBy y (filter z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z (=> x (=> x Bool))) (x2 (list x)))
      (= (nubBy z (filter y (nubBy z x2)))
        (filter y (nubBy z (nubBy z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)))
      (= (map y (as nil (list x))) (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x))) (= (elem y (++ z z)) (elem y z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (isPermutation x (++ y x))
      (isPermutation y (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (isPermutation x (++ x y))
      (isPermutation y (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (elem y (++ x2 z)) (elem y (++ z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)) (z (list Int)))
    (= (isPermutation x (++ z y)) (isPermutation x (++ y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 (list x)))
      (= (deleteBy y z (++ x2 x2)) (++ (deleteBy y z x2) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z (list x)) (x2 (list x)))
      (= (++ (filter y z) (filter y x2)) (filter y (++ z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)) (z (list x)) (x2 (list x)))
      (= (++ (map y z) (map y x2)) (map y (++ z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (++ x (deleteAll y (cons y z))) (++ x (deleteAll y z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (++ x (deleteAll y (deleteAll y z))) (++ x (deleteAll y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (++ (deleteAll x y) (deleteAll x z)) (deleteAll x (++ y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (elem y (++ z (++ z x2))) (elem y (++ z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (elem y (++ x2 (cons z x2))) (elem y (cons z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z x) (x2 (list x)))
      (= (elem z (++ x2 (filter y x2))) (elem z (++ x2 x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 (list x)))
      (= (elem z (++ x2 (nubBy y x2))) (elem z (++ x2 x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall
      ((y (=> x (=> x Bool))) (z x) (x2 (=> x Bool)) (x3 (list x)))
      (= (elem z (filter x2 (nubBy y x3))) (elem z (filter x2 x3))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (=> x (=> x Bool))) (x2 (list x)))
      (= (elem y (nubBy z (++ x2 x2))) (elem y (nubBy z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)) (z Int) (x2 (list Int)))
    (= (isPermutation x (++ y (cons z x2))) false)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (isPermutation z (deleteAll x (++ z z)))
      (isPermutation y (++ y z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)) (z (list Int)))
    (= (isPermutation (++ y x) (++ z x)) (isPermutation y z))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)))
      (= (nubBy y (++ z (nubBy y z))) (nubBy y (++ z z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z (list x)) (x2 (list x)))
      (= (nubBy y (++ (nubBy y z) x2)) (nubBy y (++ z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (elem 0 (++ (cons y x) z)) (elem 0 (++ x (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)) (x3 (list x)))
      (= (elem y (++ z (++ x3 x2))) (elem y (++ z (++ x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (elem z (++ x2 (cons z y))) (elem z (++ y (cons z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (elem z (++ y (cons z x2))) (elem z (cons z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (elem y (++ x (deleteAll y x)))
      (elem y (++ x (deleteAll y z))))))
(check-sat)
