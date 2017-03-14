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
    (isort :keep :source Sort.sort :source Sort.isort
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
                   (case (pair2 b2 ys2) (pair2 true (cons y2 ys2))))))))))))
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
(check-sat)
