(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  less :keep :source Less.less
    ((x Int) (y (list Int))) (list Int)
    (match y
      (case nil (as nil (list Int)))
      (case (cons i is) (ite (< i x) (cons i (less x is)) (less x is)))))
(define-fun-rec
  isIn :keep :source Less.isIn
    ((x Int) (y (list Int))) Bool
    (match y (case (cons i is) (or (= x i) (isIn x is)))))
(assert-not
  :source Less.prop_in
  (forall ((is (list Int)) (p Int) (x (list Int)))
    (=> (= x (less p is))
      (forall ((y Int)) (=> (isIn y x) (isIn y is))))))
(assert-not
  :source Less.prop_less
  (forall ((is (list Int)) (p Int) (x (list Int)))
    (=> (= x (less p is)) (forall ((y Int)) (=> (isIn y x) (< y p))))))
(check-sat)
