(declare-datatypes ()
  ((Nat :source Int.Nat (Z :source Int.Z)
     (S :source Int.S (proj1-S Nat)))))
(define-fun-rec
  p :keep :source Int.p
    ((x Nat) (y Nat)) Nat
    (match x
      (case Z y)
      (case (S a) (S (p a y)))))
(assert-not
  :source Int.prop_div_mod
  (forall ((a Nat) (b Nat)) (= (p a b) (p b a))))
(assert-not
  :speculated-lemma
  (forall ((x Nat)) (= (p x Z) x)))
(assert-not
  :speculated-lemma
  (forall ((x Nat)) (= (p Z x) x)))
(assert-not
  :speculated-lemma
  (forall ((x Nat) (y Nat)) (= (p y x) (p x y))))
(assert-not
  :speculated-lemma
  (forall ((x Nat) (y Nat)) (= (p (S x) y) (p x (S y)))))
(assert-not
  :speculated-lemma
  (forall ((x Nat) (y Nat)) (= (S (p x y)) (p x (S y)))))
(assert-not
  :speculated-lemma
  (forall ((x Nat) (y Nat) (z Nat)) (= (p (p x y) z) (p x (p y z)))))
(check-sat)
