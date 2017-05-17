{-# LANGUAGE ScopedTypeVariables #-}
module Naterleave where

import Tip

data Nat = Zero | Succ Nat

data TreeList = TNil | TCons Tree TreeList
data NatList = NNil | NCons Nat NatList

data Tree = Node (Tree) Nat (Tree) | Niil 



--------------------------------------------------------------------------------

{-# NOINLINE evens #-}
evens :: NatList -> NatList
evens (NCons x xs) = NCons x $ odds xs
evens NNil     = NNil

{-# NOINLINE odds #-}
odds :: NatList -> NatList
odds (NCons x xs) = evens xs
odds NNil     = NNil

interleave :: NatList -> NatList -> NatList
interleave (NCons x xs) ys = NCons x $ interleave ys xs
interleave NNil     ys = ys

--------------------------------------------------------------------------------

prop_t1 as bs = (as === interleave bs bs ) ==> (evens as === bs)

prop_evens_interleave x = (evens (interleave x x)) === x
