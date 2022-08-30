module Mean.Module.Syntax where

import qualified Mean.Core.Syntax as Core
import qualified Mean.Sugar.Syntax as Sugar

data ModuleExpr = MDecl Core.Name Sugar.SugarExpr deriving (Eq)

type Module = [ModuleExpr]