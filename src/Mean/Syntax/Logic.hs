module Mean.Syntax.Logic where

import qualified Data.Set as Set
import Mean.Var

data QExpr a
  = Univ !(Set.Set Var) !a
  | Exis !(Set.Set Var) !a
