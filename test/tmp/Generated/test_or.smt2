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
  (forall ((a Nat) (b Nat))
    (or
      (forall ((x Nat))
        (=> (= (p a (p b x)) (p (p a b) x))
          (and (= (p (S a) (p b x)) (p (p (S a) b) x))
            (= (p Z (p b x)) (p (p Z b) x)))))
      (or
        (forall ((y Nat))
          (=> (= (p y (p a b)) (p (p y a) b))
            (and (= (p y (p (S a) b)) (p (p y (S a)) b))
              (= (p y (p Z b)) (p (p y Z) b)))))
        (forall ((z Nat) (x2 Nat) (x3 Nat))
          (=> (= (p a x3) (p (p a x2) z))
            (and (= (p (S a) x3) (p (p (S a) x2) z))
              (= (p Z x3) (p (p Z x2) z)))))))))
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
