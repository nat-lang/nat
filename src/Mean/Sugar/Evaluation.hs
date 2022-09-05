{-# LANGUAGE PatternSynonyms #-}

module Mean.Sugar.Evaluation where

import qualified Data.Set as Set
import qualified Mean.Core.Encoding as CEnc
import qualified Mean.Core.Evaluation as CEval
import Mean.Core.Syntax hiding ((*), (~>))
import Mean.Sugar.Encoding
import Mean.Sugar.Patterns (pattern SFalse, pattern STrue)
import Mean.Sugar.Syntax
import qualified Mean.Sugar.Syntax as SSyn
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
  SUnOp op e -> CUnOp op (toCore e)
  SBinOp op e0 e1 -> CBinOp op (toCore e0) (toCore e1)
  STernOp op x y z -> CTernOp op (toCore x) (toCore y) (toCore z)
  -- sugar
  STree t -> toCore $ churchTree t
  SCase b cs -> case cs of
    [] -> error "empty case statement"
    [(c, e)] ->
      if c == b
        then toCore e
        else error "case without default"
    ((c, e) : cs) -> toCore $ STernOp Cond (b === c) e (SCase b cs)
  SSet es -> toCore (b ~> e)
    where
      b = SVar $ fresh (mkVar "x") (CEval.fv $ toCore <$> es)
      e = SCase b $ [(e, true) | e <- es] ++ [(b, false)]

eval :: SugarExpr -> Either CEval.EvalError CoreExpr
eval = CEval.eval . toCore

(*=) :: SugarExpr -> SugarExpr -> Bool
e0 *= e1 = CEval.confluent (toCore e0) (toCore e1)