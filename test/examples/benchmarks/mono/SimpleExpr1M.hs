module SimpleExpr1 where

import Tip

data E = E `Plus` E | EX | EY

data Tok = C | D | X | Y | Pl

--------------------------------

data TokList = TNil | TCons Tok TokList

(++*) :: TokList -> TokList -> TokList
TNil      ++* bs = bs
(TCons a as)  ++* bs = TCons a $ as ++* bs


----------------------------------

lin :: E -> TokList
lin (a `Plus` b) = (TCons C TNil) ++* lin a ++* (TCons Pl TNil) ++* lin b ++* (TCons D TNil)
lin EX        = TCons X TNil
lin EY        = TCons Y TNil

prop_unambig1 :: E -> E -> Prop 
prop_unambig1 u v = (lin u === lin v) ==> (u === v)


-------------------------------------


lin1 :: E -> TokList
lin1 (a `Plus` b) = (TCons C TNil) ++* lin1 a ++* (TCons D (TCons Pl (TCons C TNil))) ++* lin1 b ++* (TCons D TNil)
lin1 EX        = TCons X TNil
lin1 EY        = TCons Y TNil

prop_unambig2 u v = lin1 u === lin1 v ==> u === v

-----------------------------------------

lin2 :: E -> TokList
lin2 (a `Plus` b) = (TCons C TNil) ++* lin1 a ++* (TCons D (TCons Pl TNil)) ++* lin2 b ++* (TCons D TNil)
lin2 EX        = TCons X TNil
lin2 EY        = TCons Y TNil


prop_unambig3 u v = lin2 u === lin2 v ==> u === v

----------------------------------------

lin3 :: E -> TokList
lin3 (a `Plus` b) =  linTerm a ++* (TCons Pl TNil) ++* linTerm b 
lin3 EX        = TCons X TNil
lin3 EY        = TCons Y TNil

linTerm :: E -> TokList
linTerm e@(_ `Plus` _) = (TCons C TNil) ++* lin3 e ++* (TCons D TNil)
linTerm EX          = TCons X TNil
linTerm EY          = TCons Y TNil

prop_unambig4 u v = lin3 u === lin3 v ==> u === v
