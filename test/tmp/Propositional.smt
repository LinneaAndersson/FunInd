(declare-datatypes (a b)
  ((pair :source |Prelude.(,)|
     (pair2 :source |Prelude.(,)| (proj1-pair a) (proj2-pair b)))))
(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(declare-datatypes ()
  ((Form :source Propositional.Form
     (|:&:| :source |Propositional.:&:| (|proj1-:&:| Form)
       (|proj2-:&:| Form))
     (Not :source Propositional.Not (proj1-Not Form))
     (Var :source Propositional.Var (proj1-Var Int)))))
(define-fun-rec
  |\|=| :let
    ((x Int) (y (list (pair Int Bool)))) (list Bool)
    (match y
      (case nil (as nil (list Bool)))
      (case (cons z x2)
        (match z
          (case (pair2 y2 x3)
            (ite x3 (cons (= x y2) (|\|=| x x2)) (|\|=| x x2)))))))
(define-fun-rec
  or2 :let :source Prelude.or
    ((x (list Bool))) Bool
    (match x
      (case nil false)
      (case (cons y xs) (or y (or2 xs)))))
(define-fun-rec
  |\|=2| :keep :source |Propositional.\|=|
    ((x (list (pair Int Bool))) (y Form)) Bool
    (match y
      (case (|:&:| p q) (and (|\|=2| x p) (|\|=2| x q)))
      (case (Not r) (not (|\|=2| x r)))
      (case (Var z) (or2 (|\|=| z x)))))
(define-fun-rec
  okay :let
    ((x (list (pair Int Bool)))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons y xs)
        (match y (case (pair2 z y2) (cons z (okay xs)))))))
(define-fun-rec
  models7 :let
    ((x Int) (y (list (pair Int Bool)))) (list (pair Int Bool))
    (match y
      (case nil (as nil (list (pair Int Bool))))
      (case (cons z xs)
        (ite
          (distinct x (match z (case (pair2 x2 y2) x2)))
          (cons z (models7 x xs)) (models7 x xs)))))
(define-fun-rec
  models6 :let
    ((x Int) (y (list (pair Int Bool)))) (list Bool)
    (match y
      (case nil (as nil (list Bool)))
      (case (cons z x2)
        (match z
          (case (pair2 y2 x3)
            (ite x3 (models6 x x2) (cons (= x y2) (models6 x x2))))))))
(define-fun-rec
  models5 :let
    ((x Int) (y (list (pair Int Bool)))) (list (pair Int Bool))
    (match y
      (case nil (as nil (list (pair Int Bool))))
      (case (cons z xs)
        (ite
          (distinct x (match z (case (pair2 x2 y2) x2)))
          (cons z (models5 x xs)) (models5 x xs)))))
(define-fun-rec
  models4 :let
    ((x Int) (y (list (pair Int Bool)))) (list Bool)
    (match y
      (case nil (as nil (list Bool)))
      (case (cons z x2)
        (match z
          (case (pair2 y2 x3)
            (ite x3 (cons (= x y2) (models4 x x2)) (models4 x x2)))))))
(define-fun-rec
  formula2 :let
    ((p Form) (x (list (list (pair Int Bool))))) Bool
    (match x
      (case nil true)
      (case (cons y xs) (and (|\|=2| y p) (formula2 p xs)))))
(define-fun-rec
  (par (a)
    (elem :let :source Prelude.elem
       ((x a) (y (list a))) Bool
       (match y
         (case nil false)
         (case (cons z xs) (or (= z x) (elem x xs)))))))
(define-fun-rec
  okay2 :keep :source Propositional.okay
    ((x (list (pair Int Bool)))) Bool
    (match x
      (case nil true)
      (case (cons y m)
        (match y
          (case (pair2 z c) (and (not (elem z (okay m))) (okay2 m)))))))
(define-fun-rec
  formula :let
    ((x (list (list (pair Int Bool))))) Bool
    (match x
      (case nil true)
      (case (cons y xs) (and (okay2 y) (formula xs)))))
(define-fun-rec
  (par (a)
    (++ :source Prelude.++
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(define-funs-rec
  ((models3 :keep :source Propositional.models
      ((x Form) (y (list (pair Int Bool))))
      (list (list (pair Int Bool))))
   (models2 :let
      ((q Form) (x (list (list (pair Int Bool)))))
      (list (list (pair Int Bool))))
   (models :let
      ((x (list (list (pair Int Bool)))) (q Form)
       (y (list (list (pair Int Bool)))))
      (list (list (pair Int Bool)))))
  ((match x
     (case (|:&:| p q) (models2 q (models3 p y)))
     (case (Not z)
       (match z
         (case (|:&:| r q2)
           (++ (models3 (Not r) y) (models3 (|:&:| r (Not q2)) y)))
         (case (Not p2) (models3 p2 y))
         (case (Var x2)
           (ite
             (not (or2 (models4 x2 y)))
             (cons (cons (pair2 x2 false) (models5 x2 y))
               (as nil (list (list (pair Int Bool)))))
             (as nil (list (list (pair Int Bool))))))))
     (case (Var x3)
       (ite
         (not (or2 (models6 x3 y)))
         (cons (cons (pair2 x3 true) (models7 x3 y))
           (as nil (list (list (pair Int Bool)))))
         (as nil (list (list (pair Int Bool)))))))
   (match x
     (case nil (as nil (list (list (pair Int Bool)))))
     (case (cons y z) (models z q (models3 q y))))
   (match y
     (case nil (models2 q x))
     (case (cons z x2) (cons z (models x q x2))))))
(define-fun
  valid :keep :source Propositional.valid
    ((x Form)) Bool
    (match (models3 (Not x) (as nil (list (pair Int Bool))))
      (case nil true)
      (case (cons y z) false)))
(assert-not
  :source Propositional.prop_AndCommutative
  (forall ((p Form) (q Form))
    (= (valid (|:&:| p q)) (valid (|:&:| q p)))))
(assert-not
  :source Propositional.prop_AndIdempotent
  (forall ((p Form)) (= (valid (|:&:| p p)) (valid p))))
(assert-not
  :source Propositional.prop_AndImplication
  (forall ((p Form) (q Form)) (=> (valid (|:&:| p q)) (valid q))))
(assert-not
  :source Propositional.prop_Okay
  (forall ((p Form))
    (formula (models3 p (as nil (list (pair Int Bool)))))))
(assert-not
  :source Propositional.prop_Sound
  (forall ((p Form))
    (formula2 p (models3 p (as nil (list (pair Int Bool)))))))
(check-sat)
