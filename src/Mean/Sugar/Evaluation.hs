module Mean.Sugar.Evaluation where

import Mean.Sugar.Encoding
import qualified Mean.Core.Evaluation as CEval
import Mean.Core.Syntax hiding ((*), (~>))
import Mean.Sugar.Syntax
import Prelude hiding ((*))

churchTree :: ExprTree -> SugarExpr
churchTree t = case t of
  Leaf -> leaf
  Node e l r -> node * e * churchTree l * churchTree r

toCore :: SugarExpr -> CoreExpr
toCore expr = case expr of
  SLit lit -> CLit lit
  SVar var -> CVar var
  SBind b -> CBind b
  SLam (Lam b e) -> mkCLam b (toCore e)
  SApp (App e0 e1) -> mkCApp (toCore e0) (toCore e1)
  STree t -> toCore $ churchTree t

eval :: SugarExpr -> Either CEval.EvalError CoreExpr
eval = CEval.eval . toCore

(*=) :: SugarExpr -> SugarExpr -> Bool
e0 *= e1 = CEval.confluent (toCore e0) (toCore e1)