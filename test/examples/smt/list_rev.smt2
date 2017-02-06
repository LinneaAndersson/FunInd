(declare-datatypes ()
  ((List :source List_assoc.List (Nil :source List_assoc.Nil)
     (Cons :source List_assoc.Cons (proj1-Cons Int)
       (proj2-Cons List)))))
(define-fun-rec
  qrev :keep :source List_assoc.qrev
    ((x List) (y List)) List
    (match x
      (case Nil y)
      (case (Cons z xs) (qrev xs (Cons z y)))))
(assert-not
  :source List_assoc.prop_rev_assoc
  (forall ((a List)) (= (qrev Nil a) a)))
(check-sat)
