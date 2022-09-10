module Mean.Module where

import qualified Mean.Core as Core
import qualified Mean.Sugar as Sugar

data ModuleExpr = MDecl Core.Name Sugar.SugarExpr deriving (Eq)

type Module = [ModuleExpr]