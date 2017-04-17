(declare-sort sk2 0)
(declare-sort sk3 0)
(declare-sort sk4 0)
(declare-sort sk 0)
(declare-datatypes ()
  ((list2 (nil2)
     (cons2 (head2 sk) (tail2 list2)))))
(declare-datatypes ()
  ((Tree 
     (Leaf  (proj1-Leaf sk))
     (Node  (proj1-Node Tree) (proj2-Node Tree)))))
(declare-datatypes ()
  ((list  (nil )
     (cons  (head Tree) (tail list)))))
(declare-const x list)
(declare-const y list)
(declare-const z sk)
(declare-const x2 Tree)
(declare-const x3 Tree)
(declare-const x4 list)
(declare-const x5 list)
(declare-fun f1  (list) list2)
(declare-fun ++  (list list) list)
(declare-fun ++2 (list2 list2) list2)
(assert
  (= (f1 nil) nil2))
(assert
  (forall ((ts list) (z2 sk))
    (= (f1 (cons (Leaf z2) ts)) (cons2 z2 (f1 ts)))))
(assert
  (forall ((ts list) (p Tree) (q Tree))
    (= (f1 (cons (Node p q) ts)) (f1 (cons p (cons q ts))))))
(assert
  (forall ((y2 list)) (= (++ nil y2) y2)))
(assert
  (forall ((y2 list) (z2 Tree) (xs list))
    (= (++ (cons z2 xs) y2) (cons z2 (++ xs y2)))))
(assert
  (forall ((y2 list2)) (= (++2 nil2 y2) y2)))
(assert
  (forall ((y2 list2) (z2 sk) (xs list2))
    (= (++2 (cons2 z2 xs) y2) (cons2 z2 (++2 xs y2)))))
(assert
  (or
    (forall ((ps list) (qs list))
      (and (= x (cons (Node x2 x3) y))
        (=> (= (cons x2 (cons x3 y)) (++ ps qs))
          (= (f1 (cons x2 (cons x3 y))) (++2 (f1 ps) (f1 qs))))))
    (forall ((ps list) (qs list))
      (and (= x (cons (Leaf z) y))
        (=> (= y (++ ps qs)) (= (f1 y) (++2 (f1 ps) (f1 qs))))))
    (forall ((ps list) (qs list)) (= x nil))))

(assert 
  (not (= (f1 x) (++2 (f1 x4) (f1 x5)))))
(assert (!
  (= x (++ x4 x5)) :named monkey))

(check-sat)
(get-proof)
