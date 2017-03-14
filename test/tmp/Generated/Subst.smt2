(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(declare-datatypes ()
  ((Expr :source Subst.Expr (Var :source Subst.Var (proj1-Var Int))
     (Lam :source Subst.Lam (proj1-Lam Int) (proj2-Lam Expr))
     (App :source Subst.App (proj1-App Expr) (proj2-App Expr)))))
(define-fun-rec
  new-maximum :let
    ((x Int) (y (list Int))) Int
    (match y
      (case nil x)
      (case (cons z ys)
        (ite (<= x z) (new-maximum z ys) (new-maximum x ys)))))
(define-fun
  new :keep :source Subst.new
    ((x (list Int))) Int (+ (new-maximum 0 x) 1))
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
    (elem :let :source Prelude.elem
       ((x a) (y (list a))) Bool
       (match y
         (case nil false)
         (case (cons z xs) (or (= z x) (elem x xs)))))))
(define-fun-rec
  (par (a)
    (++ :source Prelude.++
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(define-fun-rec
  free :keep :source Subst.free
    ((x Expr)) (list Int)
    (match x
      (case (Var y) (cons y (as nil (list Int))))
      (case (Lam z b)
        (filter (lambda ((x2 Int)) (distinct z x2)) (free b)))
      (case (App a2 b2) (++ (free a2) (free b2)))))
(define-fun-rec
  subst :keep :source Subst.subst
    ((x Int) (y Expr) (z Expr)) Expr
    (match z
      (case (Var y2) (ite (= x y2) y (Var y2)))
      (case (Lam y3 a)
        (let ((z2 (new (++ (free y) (free a)))))
          (ite
            (= x y3) (Lam y3 a)
            (ite
              (elem y3 (free y)) (subst x y (Lam z2 (subst y3 (Var z2) a)))
              (Lam y3 (subst x y a))))))
      (case (App a2 b2) (App (subst x y a2) (subst x y b2)))))
(assert-not
  :source Subst.prop_SubstFreeNo
  (forall ((x Int) (e Expr) (a Expr) (y Int))
    (=> (not (elem x (free a)))
      (= (elem y (free a)) (elem y (free (subst x e a)))))))
(assert-not
  :source Subst.prop_SubstFreeYes
  (forall ((x Int) (e Expr) (a Expr) (y Int))
    (=> (elem x (free a))
      (=
        (elem y
          (++ (filter (lambda ((z Int)) (distinct z x)) (free a)) (free e)))
        (elem y (free (subst x e a)))))))
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
  (par (x)
    (forall ((y x) (z x) (x2 x) (x3 (list x)))
      (= (elem y (cons x2 (cons z x3)))
        (elem y (cons z (cons x2 x3)))))))
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
  (forall ((x Int)) (= (free (Var x)) (cons x (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Expr))
    (= (++ (free x) (free y)) (free (App x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int)) (= (free (Lam x (Var x))) (as nil (list Int)))))
(assert-not
  :speculated-lemma
  (= (free (Lam 0 (Var 1))) (cons 1 (as nil (list Int)))))
(assert-not
  :speculated-lemma
  (= (free (Lam 1 (Var 0))) (cons 0 (as nil (list Int)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr))
    (= (free (Lam x (Lam x y))) (free (Lam x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr))
    (= (free (Lam 1 (Lam 0 x))) (free (Lam 0 (Lam 1 x))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Expr))
    (= (free (Lam y (Lam x z))) (free (Lam x (Lam y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z (list x)) (x2 (list x)))
      (= (++ (filter y z) (filter y x2)) (filter y (++ z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (free (Lam (+ x x) (Var x))) (free (Lam 0 (Var x))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (free (Lam (+ x x) (Var 0))) (free (Lam x (Var 0))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (free (Lam (+ x x) (Var 1))) (cons 1 (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (free (Lam (+ x 1) (Var x))) (cons x (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x Int)) (= (new-maximum x (as nil (list Int))) x)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= x (new-maximum x y)) true)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (new-maximum x (cons x y)) (new-maximum x y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (new-maximum (new-maximum x y) y) (new-maximum x y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (<= 0 (new-maximum 1 x)) true)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (<= (new-maximum 1 x) 0) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (<= (new-maximum 1 x) 1) (<= (new-maximum 0 x) 1))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (new-maximum 0 (cons 1 x)) (new-maximum 1 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (new-maximum 1 (cons 0 x)) (new-maximum 1 x))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (new-maximum y (cons x z)) (new-maximum x (cons y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (new-maximum (new-maximum x z) y)
      (new-maximum (new-maximum x y) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (<= y (new-maximum x (cons y z))) true)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (<= x (new-maximum (new-maximum x y) z)) true)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (<= (new-maximum x z) (new-maximum y z))
      (<= x (new-maximum y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z Int))
    (= (<= (new-maximum z (cons x y)) z) (<= (new-maximum x y) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (new-maximum x (cons y (cons x z)))
      (new-maximum x (cons y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (new-maximum x (cons (new-maximum x y) z))
      (new-maximum (new-maximum x y) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)))
    (= (new-maximum x (cons (new-maximum y z) z))
      (new-maximum x (cons y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)) (x2 (list Int)))
    (= (new-maximum (new-maximum x z) (cons y x2))
      (new-maximum x (cons (new-maximum y z) x2)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)) (x2 (list Int)))
    (= (new-maximum (new-maximum x x2) (cons y z))
      (new-maximum x (cons (new-maximum y z) x2)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= x (+ 1 (new-maximum x y))) true)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= 0 (new-maximum (+ x x) y)) (<= 0 (new-maximum x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= 1 (new-maximum (+ x x) y))
      (<= 1 (new-maximum x (cons x y))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (<= (+ 1 (new-maximum y x)) y) false)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (new-maximum (+ x x) y) 0) (<= (new-maximum x y) 0))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (new-maximum x (cons (+ x 1) y)) (new-maximum (+ x 1) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (new-maximum (+ 1 (new-maximum x y)) y)
      (+ 1 (new-maximum (new-maximum x y) y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (new-maximum (new-maximum x y) (cons 0 z))
      (new-maximum x (cons (new-maximum 0 y) z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (new-maximum (new-maximum x y) (cons 1 z))
      (new-maximum x (cons (new-maximum 1 y) z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= 0 (new-maximum x (cons 1 y))) true)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= 1 (new-maximum x (cons 0 y))) (<= 1 (new-maximum x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (new-maximum x (cons 1 y)) 0) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (new-maximum 1 x) (new-maximum 1 y))
      (<= (new-maximum 0 x) (new-maximum 1 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (new-maximum x (cons 0 (cons 1 y)))
      (new-maximum x (cons 1 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (new-maximum x (cons 1 (cons 0 y)))
      (new-maximum x (cons 1 y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (<= (+ 1 1) (new-maximum 1 x)) (<= (+ 1 1) (new-maximum 0 x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (<= (new-maximum 1 x) (+ 1 1)) (<= (new-maximum 0 x) (+ 1 1)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Int) (x2 (list Int)))
    (= (new-maximum x (cons z (cons y x2)))
      (new-maximum x (cons y (cons z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z (list Int)) (x2 (list Int)))
    (= (new-maximum y (cons (new-maximum x z) x2))
      (new-maximum x (cons (new-maximum y z) x2)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (+ x x) (new-maximum 0 y)) (<= (+ x x) (new-maximum x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (+ x x) (new-maximum 1 y)) (<= (+ x x) (new-maximum x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (<= (new-maximum 0 x) (+ y y)) (<= (new-maximum y x) (+ y y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= 0 (+ x (new-maximum 0 y))) (<= 0 (+ x (new-maximum x y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= 1 (+ x (new-maximum 0 y))) (<= 1 (+ x (new-maximum x y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (+ x (new-maximum 0 y)) 0) (<= (+ x (new-maximum x y)) 0))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (+ x (new-maximum 1 y)) 1) (<= (+ x (new-maximum x y)) 1))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (new-maximum (+ x 1) y) 1) (<= (new-maximum (+ x x) y) 1))))
(assert-not
  :speculated-lemma
  (= (new (as nil (list Int))) 1))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (new-maximum (new x) x) (new x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (<= 0 (new x)) true)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (<= 1 (new x)) true)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (<= (new x) 0) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (new (cons 0 x)) (new x))))
(assert-not
  :speculated-lemma
  (= (new (cons 1 (as nil (list Int)))) (+ 1 1)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (new (cons (new x) x)) (+ 1 (new x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (+ 1 (new-maximum 0 x)) (new x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (<= (new x) (new-maximum y x)) (<= (new-maximum (new x) x) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (new-maximum (new-maximum (new x) y) x)
      (new-maximum (new x) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (new-maximum (new (cons x y)) y) (new (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= 1 (new-maximum (new x) y)) true)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (new-maximum 1 x) (new y)) (<= (new-maximum 0 x) (new y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (new-maximum (new x) y) 0) false)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (new-maximum x (cons (new y) (as nil (list Int))))
      (new-maximum x (cons (new y) y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (new-maximum 1 (cons (new x) y)) (new-maximum (new x) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (<= (new-maximum 1 x) (+ y y)) (<= (new x) (+ y y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (new y) (new (cons x y))) true)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (<= (new (cons x y)) (new z))
      (<= (new-maximum x y) (new-maximum 0 z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (new-maximum (+ (new x) (new y)) x) (+ (new x) (new y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (new-maximum (new x) (cons (new y) x))
      (new (cons (new-maximum 0 x) y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= 1 (+ (new x) (new y))) (<= 0 (+ (new x) (new y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int))
    (= (new-maximum 1 (cons (+ x 1) (as nil (list Int))))
      (new (cons x (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (elem (new x) x) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (new (++ x x)) (new x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (new (++ y x)) (new (++ x y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x))) (= (elem y (++ z z)) (elem y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (new-maximum x (++ y y)) (new-maximum x y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (new-maximum (new-maximum x y) z) (new-maximum x (++ y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr)) (= (subst x y (Var x)) y)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr)) (= (subst x (Var x) y) y)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (<= 1 (new-maximum 0 x)) (elem (new-maximum 1 x) x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (elem 0 (cons 1 x)) (elem 0 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (elem 1 (cons 0 x)) (elem 1 x))))
(assert-not
  :speculated-lemma
  (forall ((x Expr)) (= (new (free (Lam 0 x))) (new (free x)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr)) (= (subst 0 x (Var 1)) (Var 1))))
(assert-not
  :speculated-lemma
  (forall ((x Expr)) (= (subst 1 x (Var 0)) (Var 0))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (elem y (++ x2 z)) (elem y (++ z x2))))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Int) (z Expr))
    (= (subst y x (Lam y z)) (Lam y z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr)) (= (elem x (free (Lam x y))) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (elem (new-maximum (new y) x) y) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (elem (new x) (++ x y)) (elem (new x) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr))
    (= (new-maximum x (free (Lam x y)))
      (new-maximum x (cons x (free y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr))
    (= (new (free (subst x y y))) (new (free (App y y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int)) (= (subst x (Var y) (Var y)) (Var y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (<= (new x) (new-maximum 0 y))
      (elem (new-maximum (new x) y) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (elem 0 (cons (new x) y)) (elem 0 y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (elem (new x) (cons 0 y)) (elem (new x) y))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y (list Int)))
    (= (subst 0 x (Var (new y))) (Var (new y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Expr))
    (= (subst (new x) y (Var 0)) (Var 0))))
(assert-not
  :speculated-lemma
  (forall ((x Expr))
    (= (elem 0 (free (Lam 1 x))) (elem 0 (free x)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr))
    (= (elem 1 (free (Lam 0 x))) (elem 1 (free x)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr))
    (= (new-maximum 1 (free (Lam 0 x))) (new-maximum 1 (free x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (free (Lam (new x) (Var 0))) (cons 0 (as nil (list Int))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr) (z Expr))
    (= (subst x (subst x y y) z) (subst x y (subst x y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (<= (+ x x) (new-maximum x y)) (<= (+ x x) (new y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (elem y (++ z (++ z x2))) (elem y (++ z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x Bool)) (z x) (x2 (list x)))
      (= (elem z (++ x2 (filter y x2))) (elem z (++ x2 x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 x) (x3 (list x)))
      (= (elem y (++ (cons x2 z) x3)) (elem y (++ z (cons x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (=> x Bool)) (x2 x))
      (= (elem x2 (filter z (cons x2 y))) (@ z x2)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem x (cons (+ x x) y)) (elem x (cons 0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr))
    (= (elem x (free (subst x y y))) (elem x (free (App y y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Expr))
    (= (elem y (free (subst x z z))) (elem y (free z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr))
    (= (new-maximum x (free (subst x y y)))
      (new-maximum x (free (App y y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Expr))
    (= (new-maximum y (free (subst x z z))) (new-maximum y (free z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr) (z Expr))
    (= (free (subst x (Lam x y) z)) (free (Lam x (subst x y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr) (z Expr))
    (= (subst x z (App y (Var x))) (App (subst x z y) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr) (z Expr))
    (= (subst x y (App (Var x) z)) (App y (subst x y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Int))
    (= (subst (+ y y) x (Var y)) (subst 0 x (Var y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)) (z Expr))
    (= (subst (new (++ y x)) z z)
      (subst (new-maximum (new x) y) z z))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z Int))
    (= (<= (new-maximum 1 x) (+ y z)) (<= (new x) (+ y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (elem x (++ (cons 0 y) z)) (elem x (++ y (cons 0 z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z (list Int)))
    (= (elem x (++ (cons 1 y) z)) (elem x (++ y (cons 1 z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int))
    (= (elem x (cons (+ y x) (as nil (list Int))))
      (elem y (cons 0 (as nil (list Int)))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (elem y (cons (new-maximum y x) (as nil (list Int))))
      (<= (new-maximum y x) y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (elem 0 (++ (cons y x) z)) (elem 0 (++ x (cons y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem 0 (cons (+ x x) y)) (elem 0 (cons x (cons x y))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z (list Int)))
    (= (elem 1 (++ (cons y x) z)) (elem 1 (++ x (cons y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem 1 (cons (+ x x) y)) (elem 1 y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem (+ x x) (cons 1 y)) (elem (+ x x) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem (+ x 1) (cons x y)) (elem (+ x 1) y))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem (+ 1 (new-maximum x y)) y) false)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (elem (+ (new y) (new x)) y) false)))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem (new-maximum (+ x 1) y) y)
      (<= (+ x 1) (new-maximum x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)) (z (list Int)))
    (= (new-maximum (new x) (cons (new y) z))
      (new-maximum (new (++ x y)) z))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr))
    (= (free (subst x y (Lam 0 y))) (free (Lam 0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr))
    (= (free (subst x y (Lam 1 y))) (free (Lam 1 (subst x y y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr))
    (= (free (subst 1 y (Lam x y))) (free (Lam x (subst 1 y y))))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Expr))
    (= (subst 0 (subst 0 y x) y) (subst 0 y (subst 0 x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Int))
    (= (subst 1 x (Var (+ y y))) (Var (+ y y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr))
    (= (subst (+ x x) y (Var 0)) (subst x y (Var 0)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr)) (= (subst (+ x x) y (Var 1)) (Var 1))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Int)) (= (subst (+ y 1) x (Var y)) (Var y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y (list Int)))
    (= (elem 0 (cons (new-maximum 1 x) y)) (elem 0 y))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (elem 1 (cons (new-maximum 0 x) x)) (elem 1 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (elem 1 (cons (new-maximum 1 x) x)) (elem 1 (cons (new x) x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (elem (new-maximum 0 x) (cons 1 x))
      (elem (new-maximum 0 x) x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Expr))
    (= (elem (new x) (free (Lam 0 y))) (elem (new x) (free y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Expr))
    (= (new-maximum (new x) (free (Lam 0 y)))
      (new-maximum (new x) (free y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Expr))
    (= (new-maximum (new x) (free (Lam 1 y)))
      (new-maximum (new x) (free y)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Expr))
    (= (subst 0 x (App y (Var 1))) (App (subst 0 x y) (Var 1)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Expr))
    (= (subst 0 x (App (Var 1) y)) (App (Var 1) (subst 0 x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y (list Int)))
    (= (subst 0 x (Var (new-maximum 1 y))) (Var (new-maximum 1 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Expr))
    (= (subst 1 x (App y (Var 0))) (App (subst 1 x y) (Var 0)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Expr))
    (= (subst 1 x (App (Var 0) y)) (App (Var 0) (subst 1 x y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Expr))
    (= (subst (new-maximum 1 x) y (Var 0)) (Var 0))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Expr) (x2 Expr))
    (= (subst y z (subst x z x2)) (subst x z (subst y z x2)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Expr) (x2 Expr))
    (= (subst y (subst x z z) x2) (subst y z (subst y z x2)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr) (z Expr))
    (= (subst x (subst x z y) z) (subst x y (subst x y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)) (x3 (list x)))
      (= (elem y (++ z (++ x3 x2))) (elem y (++ z (++ x2 x3)))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr) (z Expr))
    (= (elem x (free (subst x z y))) (elem x (free (subst x y z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)) (z Int) (x2 (list Int)))
    (= (elem (new-maximum x y) (cons x y))
      (elem (new-maximum z x2) (cons z x2)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr) (z Int))
    (= (subst x (Lam z y) (Var z)) (subst x (Lam x y) (Var z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (elem (+ x x) (cons 0 y)) (elem (+ x x) (cons x y)))))
(assert-not
  :speculated-lemma
  (forall ((x Expr) (y Int))
    (= (subst 0 x (Var (+ y y))) (subst y x (Var (+ y y))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int) (z Expr))
    (= (free (subst y z (Lam x z))) (free (Lam x (subst y z z))))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Expr) (z Expr) (x2 Expr))
    (= (subst 1 (subst x y z) x2) (subst 1 z x2))))
(check-sat)
