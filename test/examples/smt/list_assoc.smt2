(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  (par (a)
    (++ :source Prelude.++
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(assert-not
  :source Int.prop_a_1
  (forall ((a (list Int)) (b (list Int)) (x (list Int)))
    (=> (= (++ a (++ b x)) (++ (++ a b) x))
      (forall ((y Int))
        (= (++ (cons y a) (++ b x)) (++ (++ (cons y a) b) x))))))
(check-sat)
