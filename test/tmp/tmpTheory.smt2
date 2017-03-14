(declare-datatypes (a)
  ((list  (nil ) (cons  (head a) (tail (list a))))))
(define-fun-rec
  ordered 
    ((x (list Int))) Bool
    (match x
      (case nil true)
      (case (cons y z)
        (match z
          (case nil true)
          (case (cons y2 xs) (and (<= y y2) (ordered (cons y2 xs))))))))
(define-fun-rec
  (par (a)
    (filter 
       ((p (=> a Bool)) (x (list a))) (list a)
       (match x
         (case nil (as nil (list a)))
         (case (cons y xs)
           (ite (@ p y) (cons y (filter p xs)) (filter p xs)))))))
(define-fun-rec
  count 
    ((x Int) (y (list Int))) Int
    (match y
      (case nil 0)
      (case (cons z ys) (ite (= x z) (+ 1 (count x ys)) (count x ys)))))
(define-fun-rec
  (par (a)
    (++ 
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (++ xs y)))))))
(define-fun-rec
  qsort 
    ((x (list Int))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons y xs)
        (++ (qsort (filter (lambda ((z Int)) (<= z y)) xs))
          (++ (cons y (as nil (list Int)))
            (qsort (filter (lambda ((x2 Int)) (> x2 y)) xs)))))))
(assert-not (forall ((xs (list Int))) (ordered (qsort xs))))
(assert-not
  (forall ((x Int) (xs (list Int)))
    (= (count x (qsort xs)) (count x xs))))
(check-sat)
(get-proof)