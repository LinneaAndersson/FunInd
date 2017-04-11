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
  ++ :keep :source Tree.++
    ((x (list Int)) (y (list Int))) (list Int)
    (match x
      (case nil y)
      (case (cons b bs) (cons b (++ bs y)))))
(define-fun-rec
  flatten0 :keep :source Tree.flatten0
    ((x Tree)) (list Int)
    (match x
      (case (Node p y q)
        (++ (++ (flatten0 p) (cons y (as nil (list Int)))) (flatten0 q)))
      (case Nil (as nil (list Int)))))
(define-fun-rec
  concatMap :keep :source Tree.concatMap
    ((x (list Tree))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons b bs) (++ (flatten0 b) (concatMap bs)))))
(assert-not
  :source Tree.prop_Flatten1
  (forall ((p Tree))
    (= (flatten1 (cons p (as nil (list Tree)))) (flatten0 p))))
(assert-not
  :source Tree.prop_Flatten1List
  (forall ((ps (list Tree))) (= (flatten1 ps) (concatMap ps))))
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
(check-sat)
