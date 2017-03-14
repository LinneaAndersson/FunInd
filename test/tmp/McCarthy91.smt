(define-fun-rec
  m :keep :source McCarthy91.m
    ((x Int)) Int (ite (> x 100) (- x 10) (m (m (+ x 11)))))
(assert-not
  :source McCarthy91.prop_M1
  (forall ((n Int)) (=> (<= n 100) (= (m n) 91))))
(assert-not
  :source McCarthy91.prop_M2
  (forall ((n Int)) (=> (>= n 101) (= (m n) (- n 10)))))
(check-sat)
