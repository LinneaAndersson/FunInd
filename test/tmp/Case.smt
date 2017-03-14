(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun
  (par (t)
    (f :keep :source Case.f
       ((x (list t))) Bool
       (match x
         (case nil true)
         (case (cons y z) true)))))
(assert-not
  :source Case.prop_f
  (par (t)
    (forall ((a (list t)))
      (match a
        (case nil true)
        (case (cons x y) true)))))
(check-sat)
