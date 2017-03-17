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
(assert-not
  :speculated-lemma
  (= (evens (as nil (list Int))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (= (odds (as nil (list Int))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int)))
    (= (evens (cons x y)) (cons x (odds y)))))
(assert-not
  :speculated-lemma
  (forall ((x Int) (y (list Int))) (= (odds (cons x y)) (evens y))))
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
  (par (x) (= (length (as nil (list x))) 0)))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (+ 1 (length z)) (length (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z Int)) (= (<= 0 (length y)) (<= z z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x))) (= (mod 0 (length (cons y z))) 0))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (<= (length (cons 0 x)) 0) (<= 1 0))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (mod (length z) (length (cons y z))) (length z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)) (x2 Int))
      (= (<= 0 (+ (length y) (length z))) (<= x2 x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)))
      (= (map y (as nil (list x))) (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)) (z x))
      (= (map y (cons z (as nil (list x))))
        (cons (@ y z) (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)) (z x) (x2 (list x)))
      (= (cons (@ y z) (map y x2)) (map y (cons z x2))))))
(assert-not
  :speculated-lemma
  (= (pairs (as nil (list Int))) (as nil (list (pair Int Int)))))
(assert-not
  :speculated-lemma
  (= (unpair (as nil (list (pair Int Int)))) (as nil (list Int))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (length (pairs x)) (length (odds x)))))
(assert-not
  :speculated-lemma
  (forall ((x (list (pair Int Int)))) (= (pairs (unpair x)) x)))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)) (z (list x)))
      (= (length (map y z)) (length z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list (pair Int Int))))
    (= (length (odds (unpair x))) (length (evens (unpair x))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int))) (= (odds (unpair (pairs x))) (odds x))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (pairs (evens (evens x))) (as nil (list (pair Int Int))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int) (z Int))
    (= (<= (+ z z) (length (evens x))) (<= y (- y z)))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)))
    (= (+ (length (odds x)) (length (odds x)))
      (length (unpair (pairs x))))))
(assert-not
  :speculated-lemma
  (forall ((x (list Int)) (y Int))
    (= (<= (length (odds x)) (length (evens x))) (<= y y))))
(check-sat)
