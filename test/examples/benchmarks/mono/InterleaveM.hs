{-# LANGUAGE ScopedTypeVariables #-}
module Naterleave where

import Tip
import Utils.Types

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

prop_Naterleave xs =
  interleave (evens xs) (odds xs) === xs
