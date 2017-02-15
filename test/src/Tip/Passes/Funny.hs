module Tip.Passes.Funny where

import Tip.Mod
import Data.List
import Tip.Core hiding (freshArgs)
import Tip.Fresh
import Tip.Types
import Tip.Parser
import Tip.Pretty.TFF
import Control.Monad
import Data.Either
import Tip.Funny.Property
import Tip.Funny.Utils


--Returns The "sub"-properties of the property
test :: Theory Id -> Expr Id -> [Expr Id]
test th e = freshFrom es th
    where 
        es = do
            apps <- findApps (thy_funcs th) e
            fIds <- freshIds apps
            sequence $ map (createPropExpr e) fIds

