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
  (par (a)
    (forall ((a1 (list a)) (b (list a)) (c (list a)))
      (= (++ a1 (++ b c)) (++ (++ a1 b) c)))))
(check-sat)
