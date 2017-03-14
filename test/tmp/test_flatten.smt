(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(declare-datatypes (a)
  ((Tree :source Test_flatten.Tree
     (Leaf :source Test_flatten.Leaf (proj1-Leaf a))
     (Node :source Test_flatten.Node (proj1-Node (Tree a))
       (proj2-Node (Tree a))))))
(define-fun-rec
  (par (a)
    (flat3 :keep :source Test_flatten.flat3
       ((x (Tree a))) (list a)
       (match x
         (case (Leaf y) (cons y (as nil (list a))))
         (case (Node z q)
           (match z
             (case (Leaf x2) (cons x2 (flat3 q)))
             (case (Node p q1) (flat3 (Node p (Node q1 q))))))))))
(define-fun-rec
  (par (a)
    (++ :source Prelude.++
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(define-fun-rec
  (par (t)
    (flat0 :keep :source Test_flatten.flat0
       ((x (Tree t))) (list t)
       (match x
         (case (Leaf y) (cons y (as nil (list t))))
         (case (Node p q) (++ (flat0 p) (flat0 q)))))))
(assert-not
  :source Test_flatten.prop_flat3
  (par (a) (forall ((p (Tree a))) (= (flat0 p) (flat3 p)))))
(check-sat)
