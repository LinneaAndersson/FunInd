(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(declare-datatypes (a)
  ((Tree :source Tree.Tree
     (Node :source Tree.Node (proj1-Node (Tree a))
       (proj2-Node a) (proj3-Node (Tree a)))
     (Nil :source Tree.Nil))))
(define-fun-rec
  (par (a)
    (flatten3 :keep :source Tree.flatten3
       ((x (Tree a))) (list a)
       (match x
         (case (Node y z r)
           (match y
             (case (Node p x2 q) (flatten3 (Node p x2 (Node q z r))))
             (case Nil (cons z (flatten3 r)))))
         (case Nil (as nil (list a)))))))
(define-fun-rec
  (par (a)
    (flatten2 :keep :source Tree.flatten2
       ((x (Tree a)) (y (list a))) (list a)
       (match x
         (case (Node p z q) (flatten2 p (cons z (flatten2 q y))))
         (case Nil y)))))
(define-fun-rec
  (par (a)
    (flatten1 :keep :source Tree.flatten1
       ((x (list (Tree a)))) (list a)
       (match x
         (case nil (as nil (list a)))
         (case (cons y ps)
           (match y
             (case (Node z x2 q)
               (match z
                 (case (Node x3 x4 x5)
                   (flatten1
                     (cons (Node x3 x4 x5) (cons (Node (as Nil (Tree a)) x2 q) ps))))
                 (case Nil (cons x2 (flatten1 (cons q ps))))))
             (case Nil (flatten1 ps))))))))
(define-fun-rec
  (par (a)
    (++ :source Prelude.++
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(define-fun-rec
  (par (a)
    (flatten0 :keep :source Tree.flatten0
       ((x (Tree a))) (list a)
       (match x
         (case (Node p y q)
           (++ (flatten0 p) (++ (cons y (as nil (list a))) (flatten0 q))))
         (case Nil (as nil (list a)))))))
(assert-not
  :source Tree.prop_Flatten1
  (par (a)
    (forall ((p (Tree a)))
      (= (flatten1 (cons p (as nil (list (Tree a))))) (flatten0 p)))))
(assert-not
  :source Tree.prop_Flatten2
  (par (a)
    (forall ((p (Tree a)))
      (= (flatten2 p (as nil (list a))) (flatten0 p)))))
(assert-not
  :source Tree.prop_PROVE_FALSE
  (par (a)
    (forall ((p (Tree a)) (a1 a))
      (= (flatten3 (Node p a1 p)) (flatten0 p)))))
(assert-not
  :source Tree.prop_Flatten3
  (par (a) (forall ((p (Tree a))) (= (flatten3 p) (flatten0 p)))))
(check-sat)
