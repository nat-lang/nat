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
import Prelude hiding ((*), (&&), (||))

churchTree :: ExprTree -> SugarExpr
churchTree t = case t of
  Leaf -> leaf
  Node e l r -> node * e * churchTree l * churchTree r

fresh :: [SugarExpr] -> SugarExpr
fresh es = go (mkVar "x") (CEval.fv $ toCore <$> es)
  where
    go v@(Var _ vPri) fv = if Set.member vPri fv
                            then go (CEval.incrVarId v) fv
                            else SVar v

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
  SSet es -> toCore (x ~> e)
    where
      x = fresh es
      e = SCase x $ [(e, true) | e <- es] ++ [(x, false)]
  SUnSetOp Comp s@(SSet es) -> let x = fresh es in toCore (x ~> not' (s * x))
  SBinSetOp op s0 s1 -> toCore $ case op of
    Mem -> s1 * s0
    op -> let x = fresh [s0,s1]
      in case op of
        Union -> x ~> (s0 * x) && (s1 * x)
        Inter -> x ~> (s0 * x) || (s1 * x)

eval :: SugarExpr -> Either CEval.EvalError CoreExpr
eval = CEval.eval . toCore

(*=) :: SugarExpr -> SugarExpr -> Bool
e0 *= e1 = CEval.confluent (toCore e0) (toCore e1)