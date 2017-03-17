(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(declare-datatypes ()
  ((Tree :source Tree.Tree
     (Node :source Tree.Node (proj1-Node Tree)
       (proj2-Node Int) (proj3-Node Tree))
     (Nil :source Tree.Nil))))
(define-fun-rec
  flatten3 :keep :source Tree.flatten3
    ((x Tree)) (list Int)
    (match x
      (case (Node y z r)
        (match y
          (case (Node p x2 q) (flatten3 (Node p x2 (Node q z r))))
          (case Nil (cons z (flatten3 r)))))
      (case Nil (as nil (list Int)))))
(define-fun-rec
  flatten2 :keep :source Tree.flatten2
    ((x Tree) (y (list Int))) (list Int)
    (match x
      (case (Node p z q) (flatten2 p (cons z (flatten2 q y))))
      (case Nil y)))
(define-fun-rec
  flatten1 :keep :source Tree.flatten1
    ((x (list Tree))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons y ps)
        (match y
          (case (Node z x2 q)
            (match z
              (case (Node x3 x4 x5)
                (flatten1 (cons (Node x3 x4 x5) (cons (Node Nil x2 q) ps))))
              (case Nil (cons x2 (flatten1 (cons q ps))))))
          (case Nil (flatten1 ps))))))
(define-fun-rec
  (par (a)
    (++ :source Prelude.++
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(define-fun-rec
  (par (a b)
    (concatMap :let :source Prelude.concatMap
       ((f (=> a (list b))) (x (list a))) (list b)
       (match x
         (case nil (as nil (list b)))
         (case (cons y xs) (++ (@ f y) (concatMap f xs)))))))
(define-fun-rec
  flatten0 :keep :source Tree.flatten0
    ((x Tree)) (list Int)
    (match x
      (case (Node p y q)
        (++ (flatten0 p) (++ (cons y (as nil (list Int))) (flatten0 q))))
      (case Nil (as nil (list Int)))))
(assert-not
  :source Tree.prop_Flatten1
  (forall ((p Tree))
    (= (flatten1 (cons p (as nil (list Tree)))) (flatten0 p))))
(assert-not
  :source Tree.prop_Flatten1List
  (forall ((ps (list Tree)))
    (= (flatten1 ps) (concatMap (lambda ((x Tree)) (flatten0 x)) ps))))
(assert-not
  :source Tree.prop_Flatten2
  (forall ((p Tree))
    (= (flatten2 p (as nil (list Int))) (flatten0 p))))
(assert-not
  :source Tree.prop_PROVE_FALSE
  (forall ((p Tree) (a Int))
    (= (flatten3 (Node p a p)) (flatten0 p))))
(assert-not
  :source Tree.prop_Flatten3
  (forall ((p Tree)) (= (flatten3 p) (flatten0 p))))
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
  (par (x)
    (forall ((y (=> x (list x))))
      (= (concatMap y (as nil (list x))) (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z (=> y (list x))) (x2 y))
      (= (concatMap z (cons x2 (as nil (list y)))) (@ z x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (list x))) (z (list x)))
      (= (++ (concatMap y z) (concatMap y z)) (concatMap y (++ z z))))))
(assert-not
  :speculated-lemma
  (= (flatten0 Nil) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Tree))
    (= (flatten0 (Node Nil x y)) (cons x (flatten0 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Tree) (y Int))
    (= (++ (flatten0 x) (cons y (as nil (list Int))))
      (flatten0 (Node x y Nil)))))
(assert-not
  :speculated-lemma
  (forall ((x Tree) (y Int) (z Tree))
    (= (++ (flatten0 x) (cons y (flatten0 z)))
      (flatten0 (Node x y z)))))
(assert-not
  :speculated-lemma
  (= (flatten1 (as nil (list Tree))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x (list Tree)))
    (= (flatten1 (cons Nil x)) (flatten1 x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (flatten2 Nil x) x)))
(assert-not
  :speculated-lemma
  (forall ((x Tree) (y Int) (z (list Int)))
    (= (flatten2 (Node x y Nil) z) (flatten2 x (cons y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Tree) (z (list Int)))
    (= (flatten2 (Node Nil x y) z) (cons x (flatten2 y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Tree) (y Int) (z Tree) (x2 (list Int)))
    (= (flatten2 x (cons y (flatten2 z x2)))
      (flatten2 (Node x y z) x2))))
(assert-not
  :speculated-lemma
  (= (flatten3 Nil) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Tree))
    (= (flatten3 (Node Nil x y)) (cons x (flatten3 y)))))
(assert-not
  :speculated-lemma
  (forall ((x Tree)) (= (flatten3 x) (flatten0 x))))
(assert-not
  :speculated-lemma
  (forall ((x Tree))
    (= (flatten2 x (as nil (list Int))) (flatten0 x))))
(assert-not
  :speculated-lemma
  (forall ((x Tree) (y (list Int)))
    (= (++ (flatten0 x) y) (flatten2 x y))))
(assert-not
  :speculated-lemma
  (forall ((x Tree) (y (list Tree)))
    (= (flatten1 (cons x y)) (flatten2 x (flatten1 y)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Tree)) (y (list Tree)))
    (= (++ (flatten1 x) (flatten1 y)) (flatten1 (++ x y)))))
(check-sat)
