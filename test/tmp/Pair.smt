(declare-datatypes (a b)
  ((pair :source |Prelude.(,)|
     (pair2 :source |Prelude.(,)| (proj1-pair a) (proj2-pair b)))))
(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  unpair :keep :source Pair.unpair
    ((x (list (pair Int Int)))) (list Int)
    (match x
      (case nil (as nil (list Int)))
      (case (cons y xys)
        (match y (case (pair2 z y2) (cons z (cons y2 (unpair xys))))))))
(define-fun-rec
  pairs :keep :source Pair.pairs
    ((x (list Int))) (list (pair Int Int))
    (match x
      (case nil (as nil (list (pair Int Int))))
      (case (cons y z)
        (match z
          (case nil (as nil (list (pair Int Int))))
          (case (cons y2 xs) (cons (pair2 y y2) (pairs xs)))))))
(define-fun-rec
  (par (a b)
    (map :let :source Prelude.map
       ((f (=> a b)) (x (list a))) (list b)
       (match x
         (case nil (as nil (list b)))
         (case (cons y xs) (cons (@ f y) (map f xs)))))))
(define-fun-rec
  (par (a)
    (length :source Prelude.length
       ((x (list a))) Int
       (match x
         (case nil 0)
         (case (cons y l) (+ 1 (length l)))))))
(define-funs-rec
  ((evens :keep :source Pair.evens ((x (list Int))) (list Int))
   (odds :keep :source Pair.odds ((x (list Int))) (list Int)))
  ((match x
     (case nil (as nil (list Int)))
     (case (cons y xs) (cons y (odds xs))))
   (match x
     (case nil (as nil (list Int)))
     (case (cons y xs) (evens xs)))))
(assert-not
  :source Pair.prop_PairUnpair
  (forall ((xs (list Int)))
    (=>
      (let ((eta (length xs)))
        (=
          (let ((md (mod eta 2)))
            (ite
              (and
                (= (ite (= eta 0) 0 (ite (<= eta 0) (- 0 1) 1))
                  (ite (<= 2 0) (- 0 (- 0 1)) (- 0 1)))
                (distinct md 0))
              (- md 2) md))
          0))
      (= (unpair (pairs xs)) xs))))
(assert-not
  :source Pair.prop_PairEvens
  (forall ((xs (list Int)))
    (=>
      (let ((eta (length xs)))
        (=
          (let ((md (mod eta 2)))
            (ite
              (and
                (= (ite (= eta 0) 0 (ite (<= eta 0) (- 0 1) 1))
                  (ite (<= 2 0) (- 0 (- 0 1)) (- 0 1)))
                (distinct md 0))
              (- md 2) md))
          0))
      (=
        (map (lambda ((x (pair Int Int))) (match x (case (pair2 y z) y)))
          (pairs xs))
        (evens xs)))))
(assert-not
  :source Pair.prop_PairOdds
  (forall ((xs (list Int)))
    (=
      (map (lambda ((x (pair Int Int))) (match x (case (pair2 y z) z)))
        (pairs xs))
      (odds xs))))
(check-sat)
