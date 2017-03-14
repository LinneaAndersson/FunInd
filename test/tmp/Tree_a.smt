(declare-datatypes ()
  ((Tree :source Tree_f.Tree
     (Leaf :source Tree_f.Leaf (proj1-Leaf Int))
     (Node :source Tree_f.Node (proj1-Node Tree) (proj2-Node Tree)))))
(declare-datatypes ()
  ((TList :source Tree_f.TList (TNil :source Tree_f.TNil)
     (TCons :source Tree_f.TCons (proj1-TCons Tree)
       (proj2-TCons TList)))))
(declare-datatypes ()
  ((List1 :source Tree_f.List1 (Nil :source Tree_f.Nil)
     (Cons :source Tree_f.Cons (proj1-Cons Int) (proj2-Cons List1)))))
(define-fun-rec
  tapp :keep :source Tree_f.tapp
    ((x TList) (y TList)) TList
    (match x
      (case TNil y)
      (case (TCons z xs) (TCons z (tapp xs y)))))
(define-fun-rec
  f1 :keep :source Tree_f.f1
    ((x TList)) List1
    (match x
      (case TNil Nil)
      (case (TCons y ts)
        (match y
          (case (Leaf z) (Cons z (f1 ts)))
          (case (Node p q) (f1 (TCons p (TCons q ts))))))))
(define-fun-rec
  app :keep :source Tree_f.app
    ((x List1) (y List1)) List1
    (match x
      (case Nil y)
      (case (Cons z xs) (Cons z (app xs y)))))
(define-fun-rec
  f0 :keep :source Tree_f.f0
    ((x Tree)) List1
    (match x
      (case (Leaf a) (Cons a Nil))
      (case (Node p q) (app (f0 p) (f0 q)))))
(assert-not
  :source Tree_f.proppy
  (forall ((ps TList) (qs TList))
    (= (f1 (tapp ps qs)) (app (f1 ps) (f1 qs)))))
(check-sat)
