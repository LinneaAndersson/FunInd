module SimpleExpr1 where

import Tip

data E = E `Plus` E | EX | EY

data Tok = C | D | X | Y | Pl

lin :: E -> [Tok]
lin (a `Plus` b) = [C] ++ lin a ++ [Pl] ++ lin b ++ [D]
lin EX        = [X]
lin EY        = [Y]

prop_unambig1 :: E -> E -> Prop
prop_unambig1 u v = (lin u === lin v) ==> (u === v)

