module Mean.Module.Syntax where

import qualified Mean.Core.Syntax as Core
import qualified Mean.Sugar.Syntax as Sugar

data Expr
  = Decl Core.Name Sugar.Expr

newtype Module = Module [Expr]