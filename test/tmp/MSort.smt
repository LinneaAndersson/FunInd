(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  ordered :keep :source MSort.ordered
    ((x (list Int))) Bool
    (match x
      (case nil true)
      (case (cons y z)
        (match z
          (case nil true)
          (case (cons y2 xs) (and (<= y y2) (ordered (cons y2 xs))))))))
(define-fun-rec
  (par (a b)
    (map :let :source Prelude.map
       ((f (=> a b)) (x (list a))) (list b)
       (match x
         (case nil (as nil (list b)))
         (case (cons y xs) (cons (@ f y) (map f xs)))))))
(define-fun-rec
  (par (a)
    (lmerge :keep :source MSort.lmerge
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z x2)
           (match y
             (case nil (cons z x2))
             (case (cons x3 x4)
               (ite
                 (<= z x3) (cons z (lmerge x2 (cons x3 x4)))
                 (cons x3 (lmerge (cons z x2) x4))))))))))
(define-fun-rec
  (par (a)
    (pairwise-pairwise1 :let :keep :source MSort.pairwise
       ((x (list (list a)))) (list (list a))
       (match x
         (case nil (as nil (list (list a))))
         (case (cons xs y)
           (match y
             (case nil (cons xs (as nil (list (list a)))))
             (case (cons ys xss)
               (cons (lmerge xs ys) (pairwise-pairwise1 xss)))))))))
(define-fun-rec
  mergingbu :keep :source MSort.mergingbu
    ((x (list (list Int)))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons xs y)
        (match y
          (case nil xs)
          (case (cons z x2)
            (mergingbu (pairwise-pairwise1 (cons xs (cons z x2)))))))))
(define-fun
  msortbu :keep :source MSort.msortbu
    ((x (list Int))) (list Int)
    (mergingbu
      (map (lambda ((y Int)) (cons y (as nil (list Int)))) x)))
(define-fun-rec
  count :keep :source MSort.count
    ((x Int) (y (list Int))) Int
    (match y
      (case nil 0)
      (case (cons z ys) (ite (= x z) (+ 1 (count x ys)) (count x ys)))))
(assert-not
  :source MSort.prop_MSortBUSorts
  (forall ((xs (list Int))) (ordered (msortbu xs))))
(assert-not
  :source MSort.prop_MSortBUCount
  (forall ((x Int) (xs (list Int)))
    (= (count x (msortbu xs)) (count x xs))))
(check-sat)
