(declare-datatypes ()
  ((Nat :source BinLists.Nat (Z :source BinLists.Z)
     (S :source BinLists.S (proj1-S Nat)))))
(declare-datatypes ()
  ((Bin :source BinLists.Bin (One :source BinLists.One)
     (ZeroAnd :source BinLists.ZeroAnd (proj1-ZeroAnd Bin))
     (OneAnd :source BinLists.OneAnd (proj1-OneAnd Bin)))))
(define-fun-rec
  s :keep :source BinLists.s
    ((x Bin)) Bin
    (match x
      (case One (ZeroAnd One))
      (case (ZeroAnd xs) (OneAnd xs))
      (case (OneAnd ys) (ZeroAnd (s ys)))))
(define-fun-rec
  plus :keep :source BinLists.plus
    ((x Bin) (y Bin)) Bin
    (match x
      (case One (s y))
      (case (ZeroAnd z)
        (match y
          (case One (s (ZeroAnd z)))
          (case (ZeroAnd ys) (ZeroAnd (plus z ys)))
          (case (OneAnd xs) (OneAnd (plus z xs)))))
      (case (OneAnd x2)
        (match y
          (case One (s (OneAnd x2)))
          (case (ZeroAnd zs) (OneAnd (plus x2 zs)))
          (case (OneAnd ys2) (ZeroAnd (s (plus x2 ys2))))))))
(define-fun-rec
  times :keep :source BinLists.times
    ((x Bin) (y Bin)) Bin
    (match x
      (case One y)
      (case (ZeroAnd xs1) (ZeroAnd (times xs1 y)))
      (case (OneAnd xs12) (plus (ZeroAnd (times xs12 y)) y))))
(define-fun-rec
  p :keep :source BinLists.p
    ((x Nat) (y Nat)) Nat
    (match x
      (case Z y)
      (case (S a) (S (p a y)))))
(define-fun-rec
  toNat :keep :source BinLists.toNat
    ((x Bin)) Nat
    (match x
      (case One (S Z))
      (case (ZeroAnd xs) (p (toNat xs) (toNat xs)))
      (case (OneAnd ys) (p (p (S Z) (toNat ys)) (toNat ys)))))
(assert-not
  :source BinLists.prop_plus
  (forall ((x Bin) (y Bin))
    (= (toNat (plus x y)) (p (toNat x) (toNat y)))))
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
(assert-not
  :speculated-lemma
  (= (toNat One) (S Z)))
(assert-not
  :speculated-lemma
  (= (toNat (ZeroAnd One)) (S (S Z))))
(assert-not
  :speculated-lemma
  (= (s One) (ZeroAnd One)))
(assert-not
  :speculated-lemma
  (forall ((x Bin)) (= (s (OneAnd x)) (ZeroAnd (s x)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin)) (= (s (ZeroAnd x)) (OneAnd x))))
(assert-not
  :speculated-lemma
  (forall ((x Bin)) (= (plus x x) (ZeroAnd x))))
(assert-not
  :speculated-lemma
  (forall ((x Bin)) (= (plus x One) (s x))))
(assert-not
  :speculated-lemma
  (forall ((x Bin)) (= (plus One x) (s x))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin)) (= (plus y x) (plus x y))))
(assert-not
  :speculated-lemma
  (forall ((x Bin)) (= (plus x (s x)) (OneAnd x))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin)) (= (plus (s x) y) (plus x (s y)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin)) (= (s (plus x y)) (plus x (s y)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin))
    (= (plus y (plus y x)) (plus x (ZeroAnd y)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin) (z Bin))
    (= (plus (plus x y) z) (plus x (plus y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin)) (= (times x One) x)))
(assert-not
  :speculated-lemma
  (forall ((x Bin)) (= (times One x) x)))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin)) (= (times y x) (times x y))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin))
    (= (times (ZeroAnd x) y) (times x (ZeroAnd y)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin))
    (= (ZeroAnd (times x y)) (times x (ZeroAnd y)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin)) (= (times x (OneAnd One)) (plus x (ZeroAnd x)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin))
    (= (plus x (times x y)) (times x (s y)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin) (z Bin))
    (= (times (times x y) z) (times x (times y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin))
    (= (plus x (times y (OneAnd x))) (plus y (times x (OneAnd y))))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin))
    (= (plus x (times y (s x))) (plus y (times x (s y))))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin) (z Bin))
    (= (plus y (plus x (times y z))) (plus x (times y (s z))))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin) (z Bin))
    (= (plus y (plus x (times z y))) (plus x (times y (s z))))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin) (z Bin))
    (= (plus (times x y) (times x z)) (times x (plus y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin) (z Bin))
    (= (plus (times y x) (times x z)) (times x (plus y z)))))
(assert-not
  :speculated-lemma
  (forall ((x Bin) (y Bin) (z Bin))
    (= (plus (times y x) (times z x)) (times x (plus y z)))))
(check-sat)
