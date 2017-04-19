{-# LANGUAGE ScopedTypeVariables #-}
module Interleave where

import Tip
import Data.List

--------------------------------------------------------------------------------
data IList = Nil | Cons Int IList 

{-# NOINLINE evens #-}
evens :: IList -> IList
evens (Cons x xs) = Cons x $ odds xs
evens Nil     = Nil

{-# NOINLINE odds #-}
odds :: IList -> IList
odds (Cons x xs) = evens xs
odds Nil     = Nil

interleave :: IList -> IList -> IList
interleave (Cons x xs) ys = Cons x $ interleave ys xs
interleave Nil     ys = ys

--------------------------------------------------------------------------------

prop_Interleave xs =
  interleave (evens xs) (odds xs) === xs
