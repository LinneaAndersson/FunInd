(define-fun-rec
  m :keep :source McCarthy91.m
    ((x Int)) Int (ite (> x 100) (- x 10) (m (m (+ x 11)))))
(assert-not
  :source McCarthy91.prop_M1
  (forall ((n Int)) (=> (<= n 100) (= (m n) 91))))
(assert-not
  :source McCarthy91.prop_M2
  (forall ((n Int)) (=> (>= n 101) (= (m n) (- n 10)))))
(assert-not
  :speculated-lemma
  (= (m 1) 91))
(assert-not
  :speculated-lemma
  (= (m 10) 91))
(assert-not
  :speculated-lemma
  (= (m 100) 91))
(assert-not
  :speculated-lemma
  (= (m 101) 91))
(assert-not
  :speculated-lemma
  (= (m 11) 91))
(assert-not
  :speculated-lemma
  (= (m 91) 91))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y Int)) (= (m (+ x y)) 91)))
(check-sat)
