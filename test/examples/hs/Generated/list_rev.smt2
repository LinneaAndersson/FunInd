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
  (forall ((a List)) (= (qrev (qrev a Nil) Nil) a)))
(assert-not
  :speculated-lemma
  (= (qrev Nil Nil) Nil))
(assert-not
  :speculated-lemma
  (forall ((x List)) (= (qrev (qrev x Nil) Nil) x)))
(assert-not
  :speculated-lemma
  (forall ((x List)) (= (qrev (qrev Nil Nil) x) x)))
(check-sat)
