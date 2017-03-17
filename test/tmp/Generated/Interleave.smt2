(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  interleave :keep :source Interleave.interleave
    ((x (list Int)) (y (list Int))) (list Int)
    (match x
      (case nil y)
      (case (cons z xs) (cons z (interleave y xs)))))
(define-funs-rec
  ((evens :keep :source Interleave.evens ((x (list Int))) (list Int))
   (odds :keep :source Interleave.odds ((x (list Int))) (list Int)))
  ((match x
     (case nil (as nil (list Int)))
     (case (cons y xs) (cons y (odds xs))))
   (match x
     (case nil (as nil (list Int)))
     (case (cons y xs) (evens xs)))))
(assert-not
  :source Interleave.prop_Interleave
  (forall ((xs (list Int)))
    (= (interleave (evens xs) (odds xs)) xs)))
(assert-not
  :speculated-lemma
  (= (evens (as nil (list Int))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (= (odds (as nil (list Int))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (evens (evens (evens x))) (evens (evens x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (evens (evens (odds x))) (evens (odds x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (evens (odds (evens x))) (odds (evens x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (evens (odds (odds x))) (odds (odds x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (odds (evens (evens x))) (as nil (list Int)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (interleave x (as nil (list Int))) x)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (interleave (as nil (list Int)) x) x)))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (evens (interleave x x)) x)))
(check-sat)
