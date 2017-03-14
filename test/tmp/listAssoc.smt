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
  :source Int.prop_a1
  (forall ((a (list Int)) (b (list Int)) (c (list Int)))
    (= (++ (++ a b) c) (++ a (++ b c)))))
(check-sat)
