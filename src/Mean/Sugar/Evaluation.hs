{-# LANGUAGE PatternSynonyms #-}
module Mean.Sugar.Evaluation where

import qualified Data.Set as Set
import qualified Mean.Core.Encoding as CEnc
import Mean.Sugar.Patterns (pattern STrue, pattern SFalse)
import Mean.Sugar.Encoding
import qualified Mean.Core.Evaluation as CEval
import Mean.Core.Syntax hiding ((*), (~>))
import qualified Mean.Sugar.Syntax as SSyn
import Mean.Sugar.Syntax
import Prelude hiding ((*))

churchTree :: ExprTree -> SugarExpr
churchTree t = case t of
  Leaf -> leaf
  Node e l r -> node * e * churchTree l * churchTree r

fresh :: Var -> Set.Set Name -> Var
fresh v fv =
  let v'@(Var _ v'Pri) = CEval.incrVId v
   in if Set.member v'Pri fv
        then fresh v' fv
        else v'

toCore :: SugarExpr -> CoreExpr
toCore expr = case expr of
  -- trivial reductions, just proxies
  SLit l -> CLit l
  SVar var -> CVar var
  SBind b -> CBind b
  SLam (Lam b e) -> mkCLam b (toCore e)
  SApp (App e0 e1) -> mkCApp (toCore e0) (toCore e1)
  SBinOp op e0 e1 -> CBinOp op (toCore e0) (toCore e1)
  SUnOp op e -> CUnOp op (toCore e)
  SCond (Cond x y z) -> CCond (Cond (toCore x) (toCore y) (toCore z))
  -- sugar
  STree t -> toCore $ churchTree t
  SCase b cs -> case cs of
    [] -> error "empty case statement"
    [(c,e)] -> if c == b
               then toCore e
               else error "case without default"
    ((c,e):cs) -> toCore $ SCond (Cond (b === c) e (SCase b cs))
  SSet es -> toCore (b ~> e) where
    b = SVar $ fresh (mkVar "x") (CEval.fv $ toCore <$> es)
    e = SCase b $ [(e, true) | e <- es] ++ [(b, false)]
  -- SSetComp

eval :: SugarExpr -> Either CEval.EvalError CoreExpr
eval = CEval.eval . toCore

(*=) :: SugarExpr -> SugarExpr -> Bool
e0 *= e1 = CEval.confluent (toCore e0) (toCore e1)