{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module A where
import qualified Text.Show.Functions
import qualified Data.Typeable as T
import qualified Prelude as P
import qualified Test.QuickCheck as QC
import qualified Tip
data Pair a b = Pair2 a b
  deriving (P.Eq, P.Ord, P.Show, T.Typeable)
data List c = Nil | Cons c (List c)
  deriving (P.Eq, P.Ord, P.Show, T.Typeable)
insert2 ::
  forall a2 .
    (QC.Arbitrary a2, QC.CoArbitrary a2, P.Ord a2) =>
      a2 -> List a2 -> List a2
insert2 x Nil = Cons x (Nil :: List a2)
insert2 x (Cons z xs) =
  case x P.<= z of
    P.True -> Cons x (Cons z xs)
    P.False -> Cons z (insert2 x xs)
isort ::
  forall a3 .
    (QC.Arbitrary a3, QC.CoArbitrary a3, P.Ord a3) =>
      List a3 -> List a3
isort Nil = Nil :: List a3
isort (Cons y ys) = insert2 y (isort ys)
bubble ::
  forall a4 .
    (QC.Arbitrary a4, QC.CoArbitrary a4, P.Ord a4) =>
      List a4 -> Pair P.Bool (List a4)
bubble Nil = Pair2 P.False (Nil :: List a4)
bubble (Cons y2 Nil) = Pair2 P.False (Cons y2 (Nil :: List a4))
bubble (Cons y2 (Cons y22 zs)) =
  case y2 P.<= y22 of
    P.True ->
      case bubble (Cons y22 zs) of
        Pair2 b22 ys22 -> Pair2 b22 (Cons y2 ys22)
    P.False ->
      case bubble (Cons y2 zs) of
        Pair2 b23 ys2 -> Pair2 P.True (Cons y22 ys2)
bubsort ::
  forall a5 .
    (QC.Arbitrary a5, QC.CoArbitrary a5, P.Ord a5) =>
      List a5 -> List a5
bubsort x2 =
  case bubble x2 of
    Pair2 b1 ys3 ->
      case b1 of
        P.True -> bubsort ys3
        P.False -> x2
prop x3 = (bubsort x3) Tip.=== (isort x3)
