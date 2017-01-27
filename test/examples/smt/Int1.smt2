(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun z :keep :source Int.z ((x Int) (y Int)) Int y)
(define-fun-rec
  i :keep :source Int.i ((x Int)) Int (ite (= x 0) 0 (i (- x 1))))
(define-fun h :keep :source Int.h ((x Int) (y Int)) Int y)
(define-fun g :keep :source Int.g ((x Int) (y Int)) Int x)
(define-fun f :keep :source Int.f ((x Int) (y Int)) Int x)
(define-fun-rec
  apa :keep :source Int.apa
    ((x Int)) (list Int)
    (ite (= x 0) (as nil (list Int)) (cons x (apa (- x 1)))))
(assert-not
  :source Int.prop_abc
  true)
(assert-not
  :source Int.prop_int
  (forall ((x Int)) (= (+ (i x) x) 1)))
(assert-not
  :source Int.prop_int2
  (forall ((x Int)) (= (> (i x) x) (< (i x) x))))
(assert-not
  :source Int.prop_apa
  (forall ((n Int)) (= (as nil (list Int)) (apa n))))
(assert-not
  :source Int.prop_div_mod
  (forall ((x Int)) (= (div x 2) (mod x 2))))
(check-sat)
