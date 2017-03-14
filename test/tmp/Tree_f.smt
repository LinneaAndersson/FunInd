(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(declare-datatypes (a)
  ((Tree :source Tree_f.Tree
     (Leaf :source Tree_f.Leaf (proj1-Leaf a))
     (Node :source Tree_f.Node (proj1-Node (Tree a))
       (proj2-Node (Tree a))))))
(define-fun-rec
  (par (t)
    (f1 :keep :source Tree_f.f1
       ((x (list (Tree t)))) (list t)
       (match x
         (case nil (as nil (list t)))
         (case (cons y ts)
           (match y
             (case (Leaf z) (cons z (f1 ts)))
             (case (Node p q) (f1 (cons p (cons q ts))))))))))
(define-fun-rec
  (par (a)
    (++ :source Prelude.++
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(define-fun-rec
  (par (t)
    (f0 :keep :source Tree_f.f0
       ((x (Tree t))) (list t)
       (match x
         (case (Leaf b) (cons b (as nil (list t))))
         (case (Node p q) (++ (f0 p) (f0 q)))))))
(assert-not
  :source Tree_f.proppy
  (par (t)
    (forall ((ps (list (Tree t))) (qs (list (Tree t))))
      (= (f1 (++ ps qs)) (++ (f1 ps) (f1 qs))))))
(check-sat)
