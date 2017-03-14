(declare-datatypes ()
  ((Nat :source Tree.Nat (Z :source Tree.Z)
     (S :source Tree.S (proj1-S Nat)))))
(declare-datatypes ()
  ((Tree :source Tree.Tree
     (Node :source Tree.Node (proj1-Node Tree)
       (proj2-Node Nat) (proj3-Node Tree))
     (Nil :source Tree.Nil))))
(declare-datatypes ()
  ((TreeList :source Tree.TreeList (TNul :source Tree.TNul)
     (TreeCons :source Tree.TreeCons (proj1-TreeCons Tree)
       (proj2-TreeCons TreeList)))))
(declare-datatypes ()
  ((List1 :source Tree.List1 (Nul :source Tree.Nul)
     (Cons :source Tree.Cons (proj1-Cons Nat) (proj2-Cons List1)))))
(define-fun-rec
  flatten3 :keep :source Tree.flatten3
    ((x Tree)) List1
    (match x
      (case (Node y z r)
        (match y
          (case (Node p x2 q) (flatten3 (Node p x2 (Node q z r))))
          (case Nil (Cons z (flatten3 r)))))
      (case Nil Nul)))
(define-fun-rec
  |concat'| :keep :source |Tree.concat'|
    ((x List1) (y List1)) List1
    (match x
      (case Nul y)
      (case (Cons z xs) (Cons z (|concat'| xs y)))))
(define-fun-rec
  flatten0 :keep :source Tree.flatten0
    ((x Tree)) List1
    (match x
      (case (Node p y q)
        (|concat'| (|concat'| (flatten0 p) (Cons y Nul)) (flatten0 q)))
      (case Nil Nul)))
(define-fun-rec
  |concatMap'| :keep :source |Tree.concatMap'|
    ((x TreeList)) List1
    (match x
      (case TNul Nul)
      (case (TreeCons t ts) (|concat'| (flatten0 t) (|concatMap'| ts)))))
(assert-not
  :source Tree.prop_Flatten3
  (forall ((p Tree)) (= (flatten3 p) (flatten0 p))))
(assert-not
  :speculated-lemma
  (forall ((x List1)) (= (|concat'| x Nul) x)))
(assert-not
  :speculated-lemma
  (forall ((x List1)) (= (|concat'| Nul x) x)))
(assert-not
  :speculated-lemma
  (forall ((x Nat) (y List1) (z List1))
    (= (|concat'| (Cons x y) z) (Cons x (|concat'| y z)))))
(assert-not
  :speculated-lemma
  (forall ((x List1) (y List1) (z List1))
    (= (|concat'| (|concat'| x y) z) (|concat'| x (|concat'| y z)))))
(assert-not
  :speculated-lemma
  (= (flatten0 Nil) Nul))
(assert-not
  :speculated-lemma
  (forall ((x Nat) (y Tree))
    (= (flatten0 (Node Nil x y)) (Cons x (flatten0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Tree) (y Nat))
    (= (|concat'| (flatten0 x) (Cons y Nul))
      (flatten0 (Node x y Nil)))))
(assert-not
  :speculated-lemma
  (forall ((x Tree) (y Nat) (z Tree))
    (= (|concat'| (flatten0 x) (Cons y (flatten0 z)))
      (flatten0 (Node x y z)))))
(assert-not
  :speculated-lemma
  (= (|concatMap'| TNul) Nul))
(assert-not
  :speculated-lemma
  (forall ((x Tree))
    (= (|concatMap'| (TreeCons x TNul)) (flatten0 x))))
(assert-not
  :speculated-lemma
  (forall ((x TreeList))
    (= (|concatMap'| (TreeCons Nil x)) (|concatMap'| x))))
(assert-not
  :speculated-lemma
  (forall ((x Tree) (y TreeList))
    (= (|concat'| (flatten0 x) (|concatMap'| y))
      (|concatMap'| (TreeCons x y)))))
(assert-not
  :speculated-lemma
  (= (flatten3 Nil) Nul))
(assert-not
  :speculated-lemma
  (forall ((x Nat) (y Tree))
    (= (flatten3 (Node Nil x y)) (Cons x (flatten3 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Tree)) (= (flatten3 x) (flatten0 x))))
(check-sat)
