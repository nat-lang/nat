module Mean.Sugar.EvaluationSpec where

import Mean.Core.Syntax
import Mean.Core.Encoding
import qualified Mean.Core.Evaluation as CEval
import Mean.Sugar.Syntax
import qualified Mean.Sugar.Evaluation as SEval
import Mean.Sugar.Parser
import Mean.Common.Parser (parse)
import Test.Hspec
import Test.HUnit ((@?=))

e0 @= e1 = (e0 CEval.@= e1) @?= True

p = parse pSExpr

{-
functionApplication :: TypeCheckedExpr -> TypeCheckedExpr -> Maybe S.Expr
functionApplication e0 e1 = case (e0, e1) of
  (FnNode fn tDom, TypedExpr arg t) | Inf.unifiable tDom t -> doFA fn arg
  (TypedExpr arg t, FnNode fn tDom) | Inf.unifiable tDom t -> doFA fn arg
  _ -> Nothing
  where
    doFA fn arg = Just $ S.App fn arg

pattern BinderNode b <- TypedExpr (S.EBinder b) _

predicateAbstraction :: TypeCheckedExpr -> TypeCheckedExpr -> Maybe S.Expr
predicateAbstraction e0 e1 = case (e0, e1) of
  (BinderNode b, TypedExpr e t) -> doPA b e
  (TypedExpr e t, BinderNode b) -> doPA b e
  _ -> Nothing
  where
    doPA b e = Just $ S.Lam b e

predicateModification :: TypeCheckedExpr -> TypeCheckedExpr -> Maybe S.Expr
predicateModification e0 e1 = case (e0, e1) of
  (TypedExpr e t, TypedExpr e' t') | Inf.unifiable t t' -> doPM e e'
  _ -> Nothing
  where
    doPM e e' = Just $ S.EBinOp S.Conj e e'
-}

spec :: Spec
spec = do
  describe "alpha equivalence (@=)" $ do
    it "recognizes alpha equivalent church trees with alpha equivalent expressive nodes" $ do
      let (Right t0) = SEval.eval (STree (Node (x ~> x) Leaf Leaf))
      let (Right t1) = SEval.eval (STree (Node (y ~> y) Leaf Leaf))

      t0 @= t1

  describe "confluence (*=)" $ do
    it "equates folded trees with expressions" $ do
      let (Right t) = p "[S [\\x.x] [x]]"
      let (Right fn) = p "\\"
     --  SEval.eval
