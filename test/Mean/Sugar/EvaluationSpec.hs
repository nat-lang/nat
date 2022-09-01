module Mean.Sugar.EvaluationSpec where

import qualified Mean.Core.Encoding as CEnc
import qualified Mean.Core.Evaluation as CEval
import Mean.Core.Viz
import Mean.Core.Syntax hiding ((*), (~>))
import qualified Mean.Core.Syntax as CSyn
import Mean.Sugar.Evaluation hiding ((*=), (@=))
import qualified Mean.Sugar.Evaluation as SEval
import Mean.Sugar.Encoding
import Mean.Sugar.Syntax
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding ((*), id)
import Debug.Trace (traceM)

e0 @= e1 = (e0 CEval.@= e1) @?= True
e0 *= e1 = (e0 SEval.*= e1) @?= True

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
  let t = STree (Node (mkSVar "S") (Node (x ~> x) Leaf Leaf) (Node y Leaf Leaf))

  describe "alpha equivalence (@=)" $ do
    it "equates alpha equivalent church trees with alpha equivalent expressive nodes" $ do
      let (Right t0) = eval (STree (Node (x ~> x) Leaf Leaf))
      let (Right t1) = eval (STree (Node (y ~> y) Leaf Leaf))

      t0 @= t1

  describe "eval" $ do
    it "reduces sugar trees to their church encodings" $ do
      eval t `shouldBe` eval (node * mkSVar "S" * (node * (x ~> x) * leaf * leaf) * (node * y * leaf * leaf))

    it "reduces folds over church trees" $ do
      -- FA composition, l(r)
      let foldLR = x ~> (l ~> (r ~> (z ~> ((l * x) * (r * x)))))

      eval (t * leaf * fold) `shouldBe` eval (z ~> ((x ~> x) * y))
