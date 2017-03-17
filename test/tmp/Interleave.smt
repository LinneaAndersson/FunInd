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
(check-sat)
