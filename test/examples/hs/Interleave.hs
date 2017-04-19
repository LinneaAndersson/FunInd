{-# LANGUAGE ScopedTypeVariables #-}
module Interleave where

import Tip
import Data.List

--------------------------------------------------------------------------------

{-# NOINLINE evens #-}
evens :: [Int] -> [Int]
evens (x:xs) = x : odds xs
evens []     = []

{-# NOINLINE odds #-}
odds :: [Int] -> [Int]
odds (x:xs) = evens xs
odds []     = []

interleave :: [Int] -> [Int] -> [Int]
interleave (x:xs) ys = x : interleave ys xs
interleave []     ys = ys

--------------------------------------------------------------------------------

prop_Interleave xs =
  interleave (evens xs) (odds xs) === xs

prop_evens xs =
  interleave (evens xs) xs === xs

prop_evensX2 xs =
  interleave (evens xs) (evens xs) === xs

prop_FALSE xs =
  interleave xs xs === xs
