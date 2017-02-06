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
  :source Int.prop_a_1
  (forall ((a Nat) (b Nat) (c Nat)) (= (p a (p b c)) (p (p a b) c))))
(check-sat)
