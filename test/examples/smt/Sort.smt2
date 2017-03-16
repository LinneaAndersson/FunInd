(declare-datatypes (a b)
  ((pair :source |Prelude.(,)|
     (pair2 :source |Prelude.(,)| (proj1-pair a) (proj2-pair b)))))
(declare-datatypes (a)
  ((list :source |Prelude.[]| (nil :source |Prelude.[]|)
     (cons :source |Prelude.:| (head a) (tail (list a))))))
(define-fun-rec
  (par (a)
    (uniqsorted-uniqsorted1 :let :keep :source SortUtils.uniqsorted
       ((x (list a))) Bool
       (match x
         (case nil true)
         (case (cons y z)
           (match z
             (case nil true)
             (case (cons y2 xs)
               (and (< y y2) (uniqsorted-uniqsorted1 (cons y2 xs))))))))))
(define-fun-rec
  (par (a)
    (ordered-ordered1 :let :keep :source SortUtils.ordered
       ((x (list a))) Bool
       (match x
         (case nil true)
         (case (cons y z)
           (match z
             (case nil true)
             (case (cons y2 xs)
               (and (<= y y2) (ordered-ordered1 (cons y2 xs))))))))))
(define-fun-rec
  (par (a)
    (insert :keep :source Sort.insert
       ((x a) (y (list a))) (list a)
       (match y
         (case nil (cons x (as nil (list a))))
         (case (cons z xs)
           (ite (<= x z) (cons x (cons z xs)) (cons z (insert x xs))))))))
(define-fun-rec
  (par (a)
    (isort :keep :source Sort.sort
       ((x (list a))) (list a)
       (match x
         (case nil (as nil (list a)))
         (case (cons y xs) (insert y (isort xs)))))))
(define-fun-rec
  (par (a)
    (elem :let :source Prelude.elem
       ((x a) (y (list a))) Bool
       (match y
         (case nil false)
         (case (cons z xs) (or (= z x) (elem x xs)))))))
(define-fun-rec
  (par (a)
    (deleteBy :source Data.List.deleteBy
       ((x (=> a (=> a Bool))) (y a) (z (list a))) (list a)
       (match z
         (case nil (as nil (list a)))
         (case (cons y2 ys)
           (ite (@ (@ x y) y2) ys (cons y2 (deleteBy x y ys))))))))
(define-fun-rec
  (par (a)
    (isPermutation :keep :source SortUtils.isPermutation
       ((x (list a)) (y (list a))) Bool
       (match x
         (case nil
           (match y
             (case nil true)
             (case (cons z x2) false)))
         (case (cons x3 xs)
           (and (elem x3 y)
             (isPermutation xs
               (deleteBy (lambda ((x4 a)) (lambda ((x5 a)) (= x4 x5)))
                 x3 y))))))))
(define-fun-rec
  (par (a)
    (deleteAll :keep :source SortUtils.deleteAll
       ((x a) (y (list a))) (list a)
       (match y
         (case nil (as nil (list a)))
         (case (cons z ys)
           (ite (= x z) (deleteAll x ys) (cons z (deleteAll x ys))))))))
(define-fun-rec
  (par (a)
    (count :keep :source SortUtils.count
       ((x a) (y (list a))) Int
       (match y
         (case nil 0)
         (case (cons z ys)
           (ite (= x z) (+ 1 (count x ys)) (count x ys)))))))
(define-fun-rec
  (par (a)
    (bubble :keep :source Sort.bubble
       ((x (list a))) (pair Bool (list a))
       (match x
         (case nil (pair2 false (as nil (list a))))
         (case (cons y z)
           (match z
             (case nil (pair2 false (cons y (as nil (list a)))))
             (case (cons y2 xs)
               (ite
                 (<= y y2)
                 (match (bubble (cons y2 xs))
                   (case (pair2 b22 ys22) (pair2 b22 (cons y ys22))))
                 (match (bubble (cons y xs))
                   (case (pair2 b23 ys2) (pair2 true (cons y2 ys2))))))))))))
(define-fun-rec
  (par (a)
    (bubsort :keep :source Sort.bubsort
       ((x (list a))) (list a)
       (match (bubble x) (case (pair2 b1 ys) (ite b1 (bubsort ys) x))))))
(assert-not
  :source Sort.prop_BubSortSorts
  (forall ((x (list Int))) (ordered-ordered1 (bubsort x))))
(assert-not
  :source Sort.prop_BubSortCount
  (forall ((x Int) (y (list Int)))
    (= (count x (bubsort y)) (count x y))))
(assert-not
  :source Sort.prop_BubSortPermutes
  (forall ((x (list Int))) (isPermutation (bubsort x) x)))
(assert-not
  :source Sort.prop_BubSortIsSort
  (forall ((x (list Int))) (= (bubsort x) (isort x))))
(assert-not
  :speculated-lemma
  (par (x) (= (bubsort (as nil (list x))) (as nil (list x)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x))) (= (bubsort (bubsort y)) (bubsort y)))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y (list x))) (= (< y (bubsort y)) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (< (as nil (list x)) (bubsort y)) (< (as nil (list x)) y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (<= (bubsort y) (as nil (list x))) (<= y (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x))
      (= (bubsort (cons y (as nil (list x))))
        (cons y (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)))
      (= (<= (bubsort y) y) (<= (bubsort z) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (bubsort (cons y (bubsort z))) (bubsort (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (< y (cons z (bubsort x2))) (< y (cons z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= z (bubsort (cons y z))) (< z (bubsort (cons y z)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (<= (cons y (bubsort z)) x2) (<= (cons y z) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (bubsort (cons y z)) z) (< (bubsort (cons y z)) z)))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z (list y)) (x2 (pair x (list y))) (x3 x))
      (= (<= x2 (pair2 x3 (bubsort z)))
        (<= x2 (pair2 x3 (as nil (list y))))))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z x) (x2 (list y)))
      (= (<= (pair2 z (as nil (list y))) (pair2 z x2))
        (<= (bubsort x2) x2)))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y x)) (= (count y (as nil (list x))) 0))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 Int))
      (= (< x2 (count y z)) (< x2 0)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 Int))
      (= (<= (count y z) x2) (<= 0 x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (+ 1 (count y z)) (count y (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x))
      (= (count z (cons y (as nil (list x))))
        (count y (cons z (as nil (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 Int) (x3 Int))
      (= (< x2 (+ x3 (count y z))) (<= (+ x2 1) x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x))
      (= (deleteAll y (as nil (list x))) (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (deleteAll y (cons y z)) (deleteAll y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (deleteAll y (deleteAll y z)) (deleteAll y (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (deleteAll z (deleteAll y x2))
        (deleteAll y (deleteAll z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (< x2 (deleteAll z (deleteAll y x2)))
        (< x2 (deleteAll z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (cons y (deleteAll y z)) z)
        (<= (cons y (as nil (list x))) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (<= (deleteAll z (deleteAll y x2)) x2)
        (<= (deleteAll z x2) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (deleteAll y (cons z (cons y x2))) (deleteAll y (cons z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (deleteAll z (cons y (deleteAll z x2)))
        (deleteAll z (cons y (cons z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (deleteAll y (deleteAll z (cons y x2)))
        (deleteAll y (cons y (deleteAll z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)) (x3 (list x)))
      (= (<= (cons z (deleteAll y x2)) x3)
        (<= (cons z (deleteAll z x2)) x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x))
      (= (deleteBy y z (as nil (list x))) (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (as (deleteBy < y (cons y z)) (list x))
        (cons y (as (deleteBy < y z) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (as (deleteBy <= y (cons y z)) (list x)) z))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y x)) (= (elem y (as nil (list x))) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (elem y (cons y (as nil (list x)))) (elem y (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (elem y (cons y x2)) (elem z (cons z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x))
      (= (elem z (cons y (as nil (list x))))
        (elem y (cons z (as nil (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (elem z (cons y (cons z x2))) (elem z (cons z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (elem y (cons z (cons z x2))) (elem y (cons z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 x) (x3 (list x)))
      (= (elem y (cons x2 (cons z x3)))
        (elem y (cons z (cons x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (isPermutation (as nil (list x)) y)
        (isPermutation y (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (isPermutation (as nil (list x)) (as nil (list x)))
        (isPermutation y y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)))
      (= (isPermutation z y) (isPermutation y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (elem y (cons y z)) (isPermutation z z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (isPermutation z (cons y z)) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (isPermutation (as nil (list x)) (cons y z)) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (elem y (as (deleteBy < y z) (list x))) (elem y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (isPermutation x2 (cons y (cons z x2))) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 x) (x3 (list x)))
      (= (isPermutation y (cons z (cons x2 x3))) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (isPermutation (cons y z) (cons y (as nil (list x))))
        (isPermutation z (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 x))
      (= (isPermutation (cons y (as nil (list x))) (cons x2 z))
        (isPermutation (cons y z) (cons x2 (as nil (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (=
        (isPermutation (cons y (as nil (list x)))
          (cons z (as nil (list x))))
        (isPermutation (cons y x2) (cons z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 x) (x3 (list x)))
      (= (isPermutation (cons y x3) (cons x2 z))
        (isPermutation (cons y z) (cons x2 x3))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 x) (x3 (list x)))
      (= (isPermutation (cons y z) (cons y x3))
        (isPermutation (cons x2 z) (cons x2 x3))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x))
      (= (insert y (as nil (list x))) (cons y (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= z (insert y z)) (< z (insert y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (insert y z) z) (< (insert y z) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (insert y (cons y z)) (cons y (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (insert z (insert y x2)) (insert y (insert z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x))
      (= (insert z (cons y (as nil (list x))))
        (insert y (cons z (as nil (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x x)) (z x) (x2 (list x)))
      (= (< (as nil (list x)) (insert (@ y z) x2))
        (< (as nil (list x)) (insert z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (< z (insert y (insert y z))) (< z (insert y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (< (insert y (insert y z)) z) (< (insert y z) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (<= x2 (insert y (cons z x2))) (< x2 (insert y (cons z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (<= x2 (insert z (cons y x2))) (< x2 (insert y (cons z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 x) (x3 (list x)))
      (= (<= y (insert z (cons x2 x3))) (< y (insert z (cons x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (<= y (insert z (insert z x2)))
        (< y (insert z (insert z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (<= x2 (insert y (insert z x2)))
        (< x2 (insert y (insert z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 x) (x3 (list x)))
      (= (<= y (insert z (insert x2 x3)))
        (< y (insert z (insert x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (<= (insert y (cons z x2)) x2) (< (insert y (cons z x2)) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)) (x3 (list x)))
      (= (<= (insert y (cons z x2)) x3) (< (insert y (cons z x2)) x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (<= (insert z (cons y x2)) x2) (< (insert y (cons z x2)) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (<= (insert y (insert y z)) x2)
        (< (insert y (insert y z)) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (<= (insert y (insert z x2)) x2)
        (< (insert y (insert z x2)) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)) (x3 (list x)))
      (= (<= (insert y (insert z x2)) x3)
        (< (insert y (insert z x2)) x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (insert y (insert z (cons z x2)))
        (insert y (cons z (cons z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (insert y (insert z (cons z x2)))
        (insert y (cons z (cons z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (cons y (as nil (list x))) (insert y z))
        (<= (cons y z) (insert y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 x) (x3 (list x)))
      (= (< x3 (insert z (cons x2 y))) (< x3 (insert z (cons x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 x) (x3 (list x)))
      (= (< (insert z (cons x2 y)) x3) (< (insert z (cons x2 x3)) x3)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (insert z (insert y (cons z x2)))
        (insert y (cons z (cons z x2)))))))
(assert-not
  :speculated-lemma
  (par (x) (= (isort (as nil (list x))) (as nil (list x)))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y (list x))) (= (isort (isort y)) (isort y)))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y (list x))) (= (< y (isort y)) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (isort (cons y z)) (insert y (isort z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (isort (insert y z)) (insert y (isort z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (< (as nil (list x)) (isort y)) (< (as nil (list x)) y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)))
      (= (<= (isort y) y) (<= (isort z) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (<= y (insert z x2)) (< y (insert z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (<= (insert y z) x2) (< (insert y z) x2)))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z (list y)) (x2 (pair x (list y))) (x3 x))
      (= (<= x2 (pair2 x3 (isort z)))
        (<= x2 (pair2 x3 (as nil (list y))))))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z y) (x2 (list y)) (x3 (pair x (list y))) (x4 x))
      (= (< x3 (pair2 x4 (insert z x2)))
        (<= x3 (pair2 x4 (as nil (list y))))))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z y) (x2 (list y)) (x3 x) (x4 (pair x (list y))))
      (= (<= (pair2 x3 (insert z x2)) x4)
        (< (pair2 x3 (as nil (list y))) x4)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (< (insert y (isort z)) (isort z)) (< (insert y z) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x))
      (= (ordered-ordered1 (cons y (as nil (list x))))
        (ordered-ordered1 (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (ordered-ordered1 (cons y (cons y z)))
        (ordered-ordered1 (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x))
      (= (uniqsorted-uniqsorted1 (cons y (as nil (list x))))
        (uniqsorted-uniqsorted1 (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (uniqsorted-uniqsorted1 (cons y (cons y z))) false))))
(assert-not
  :speculated-lemma
  (par (x) (forall ((y (list x))) (= (isort y) (bubsort y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (= (uniqsorted-uniqsorted1 (as nil (list x)))
      (ordered-ordered1 (as nil (list x))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (ordered-ordered1 (bubsort y))
        (ordered-ordered1 (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (<= y (bubsort y)) (ordered-ordered1 y)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (count y (bubsort z)) (count y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x))) (= (elem y (bubsort z)) (elem y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)))
      (= (isPermutation y (bubsort y))
        (ordered-ordered1 (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z (list x)))
      (= (isPermutation (bubsort y) z) (isPermutation y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (bubsort (deleteAll y z)) (deleteAll y (bubsort z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x))) (= (< z (deleteAll y z)) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (< (deleteAll y z) z) (elem y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (deleteAll y z) z) (ordered-ordered1 (as nil (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x))) (= (count y (deleteAll y z)) 0))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (deleteAll y (insert y z)) (deleteAll y (cons y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (isPermutation z (deleteAll y z)) (<= z (deleteAll y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (ordered-ordered1 (as (deleteBy <= y z) (list x)))
        (ordered-ordered1 (as (deleteBy < y z) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (uniqsorted-uniqsorted1 (as (deleteBy <= y z) (list x)))
        (uniqsorted-uniqsorted1 (as (deleteBy < y z) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (=> x (=> x Bool))) (x2 x) (x3 (list x)))
      (= (deleteBy z x2 (deleteAll y x3))
        (deleteAll y (deleteBy z x2 x3))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (< z (deleteAll y (bubsort z))) (< z (deleteAll y z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (< z (as (deleteBy <= y z) (list x)))
        (< z (as (deleteBy < y z) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (< (bubsort z) (deleteAll y z)) (< (bubsort z) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (deleteAll y z) (bubsort z)) (ordered-ordered1 z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (deleteAll y (bubsort z)) z) (<= (deleteAll y z) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (as (deleteBy <= y z) (list x)) z)
        (<= (as (deleteBy < y z) (list x)) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (count y (as (deleteBy < y z) (list x))) (count y z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (deleteAll y (as (deleteBy <= y z) (list x)))
        (deleteAll y (as (deleteBy < y z) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 (list x)))
      (= (bubsort (deleteBy y z (bubsort x2)))
        (deleteBy y z (bubsort x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (as (deleteBy <= y (insert y z)) (list x)) z))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (< (deleteAll z y) (deleteAll z x2)) (< y (deleteAll z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (< (insert y (deleteAll y z)) x2) (< (insert y z) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (pair x (list x))) (z x) (x2 (list x)))
      (= (< y (pair2 z (deleteAll z x2)))
        (< y (pair2 z (bubsort x2)))))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z y) (x2 (list y)) (x3 x) (x4 (pair x (list y))))
      (= (< (pair2 x3 (deleteAll z x2)) x4)
        (< (pair2 x3 (as nil (list y))) x4)))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z y) (x2 (list y)) (x3 (pair x (list y))) (x4 x))
      (= (<= x3 (pair2 x4 (deleteAll z x2)))
        (<= x3 (pair2 x4 (as nil (list y))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (pair x (list x))))
      (= (<= (pair2 y (deleteAll y z)) x2)
        (<= (pair2 y (bubsort z)) x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 x))
      (= (count z (cons x2 (deleteAll z y)))
        (count z (cons x2 (as nil (list x))))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 x) (x3 (list x)))
      (= (count y (deleteAll z (insert x2 x3)))
        (count y (deleteAll z (cons x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (=> x (=> x Bool))) (x2 x) (x3 (list x)))
      (= (count y (deleteBy z x2 (bubsort x3)))
        (count y (deleteBy z x2 x3))))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z x) (x2 (=> y x)) (x3 y) (x4 (list x)))
      (= (count z (as (deleteBy <= (@ x2 x3) x4) (list x)))
        (count z (as (deleteBy < (@ x2 x3) x4) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z (=> y x)) (x2 y) (x3 x) (x4 (list x)))
      (= (count (@ z x2) (as (deleteBy <= x3 x4) (list x)))
        (count (@ z x2) (as (deleteBy < x3 x4) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (deleteAll y (insert z (deleteAll y x2)))
        (deleteAll y (cons y (insert z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 (list x)))
      (= (isPermutation y (cons z (insert z x2))) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (isPermutation x2 (cons y (insert z x2))) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 x) (x3 (list x)))
      (= (isPermutation y (cons z (insert x2 x3))) false))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 (list x)))
      (= (isPermutation x2 (deleteAll y (insert z x2)))
        (isPermutation x2 (deleteAll y (cons z x2)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (list x)) (z x) (x2 x) (x3 (list x)))
      (= (isPermutation y (deleteAll z (insert x2 x3)))
        (isPermutation y (deleteAll z (cons x2 x3)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y (=> x (=> x Bool))) (z x) (x2 (list x)))
      (= (isPermutation x2 (deleteBy y z (bubsort x2)))
        (isPermutation x2 (deleteBy y z x2))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)) (x2 (list x)))
      (= (isPermutation (cons y z) (cons y x2)) (isPermutation z x2)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (< (bubsort z) (as (deleteBy <= y z) (list x)))
        (< (bubsort z) (as (deleteBy < y z) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (as (deleteBy <= y z) (list x)) (bubsort z))
        (<= (as (deleteBy < y z) (list x)) (bubsort z))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (<= (as (deleteBy <= y (bubsort z)) (list x)) z)
        (<= (as (deleteBy < y (bubsort z)) (list x)) z)))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (bubsort (as (deleteBy < y (insert y z)) (list x)))
        (bubsort (as (deleteBy < y (cons y z)) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (as (deleteBy < y (insert y (bubsort z))) (list x))
        (insert y (as (deleteBy < y (bubsort z)) (list x)))))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z (list x)))
      (= (as (deleteBy <= y (as (deleteBy <= y z) (list x))) (list x))
        (as (deleteBy <= y (as (deleteBy < y z) (list x))) (list x))))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z (=> y x)) (x2 y) (x3 (=> y x)) (x4 y) (x5 (list x)))
      (= (count (@ x3 x2) (deleteAll (@ z x4) x5))
        (count (@ x3 x2) (deleteAll (@ x3 x4) x5))))))
(assert-not
  :speculated-lemma
  (par (x y)
    (forall ((z x) (x2 (=> x y)) (x3 x) (x4 (list y)))
      (= (count (@ x2 z) (deleteAll (@ x2 x3) x4)) 0))))
(assert-not
  :speculated-lemma
  (par (x)
    (forall ((y x) (z x) (x2 x) (x3 (list x)))
      (= (count z (deleteAll y (deleteAll x2 x3)))
        (count z (deleteAll x2 x3))))))
(check-sat)
