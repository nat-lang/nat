module Mean.Sugar.Evaluation where

import qualified Mean.Core.Evaluation as CEval
import Mean.Core.Syntax
import Mean.Sugar.Syntax
import Mean.Sugar.Encoding
import Prelude hiding ((*))

churchTree :: ExprTree -> CoreExpr
churchTree t = case t of
  Leaf -> leaf
  Node e' l' r' -> node * e' * churchTree l' * churchTree r'

toCore :: SugarExpr -> CoreExpr
toCore expr = case expr of
  SLit lit -> CLit lit
  SVar var -> CVar var
  SLam (Lam b e) -> mkCLam b (toCore e)
  SApp (App e0 e1) -> mkCApp (toCore e0) (toCore e1)
  STree t -> churchTree t

reduce :: SugarExpr -> CEval.Evaluation CoreExpr
reduce = CEval.reduce . toCore
