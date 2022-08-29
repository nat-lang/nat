{-# LANGUAGE PatternSynonyms #-}

module Interpreter.Composition
  ( ExprTree,
    ExprLabel (..),
    TypeCheckedExpr (..),
    TypeCheckedExprLabel,
    SemanticTree,
    SemanticLabel (..),
    EvaluatedExpr (..),
    compose,
    collectCompositionErrors,
  )
where

import qualified Compiler.Core.Inference as Inf
import Compiler.Core.Pretty
import qualified Compiler.Core.Syntax as S
import qualified Compiler.Core.TypeEnv as TyEnv
import Compiler.Tree.Syntax as T
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldl', toList)
import qualified Data.Map as Map
import Debug.Trace (traceM)
import qualified Interpreter.Evaluation as Sem
import qualified Interpreter.Fragment as Frag
import Utils

-- | Initial composition: instantiate lexical entries.
type ExprLabel = (Maybe S.Expr, ConstituencyLabel)

type ExprTree = T.Tree ExprLabel

-- | Intermediate composition: assign types, do transformations.
data TypeCheckedExpr = TypedExpr S.Expr S.Type | UntypedExpr S.Expr Inf.TypeError

type TypeCheckedExprLabel = (Maybe TypeCheckedExpr, ConstituencyLabel)

type TypeCheckedExprTree = T.Tree TypeCheckedExprLabel

-- | Evaluated composition: evaluate type checked expressions.
type EvaluatedExpr = Either Sem.EvalError Sem.Value

type SemanticLabel = (Maybe EvaluatedExpr, Maybe TypeCheckedExpr, ConstituencyLabel)

type SemanticTree = T.Tree SemanticLabel

type FragmentCtx = Reader Frag.Fragment

instance Show TypeCheckedExpr where
  show (TypedExpr e ty) = show e ++ ": " ++ show ty
  show (UntypedExpr e err) = show e ++ ": " ++ show err

data CompositionError = CEvalError Sem.EvalError | CTypeError Inf.TypeError

collectCompositionErrors :: SemanticTree -> [SemanticLabel]
collectCompositionErrors = filter incl . toList
  where
    incl node = case node of
      (Just (Left _), _, _) -> True
      (_, Just (UntypedExpr _ _), _) -> True
      _ -> False

checkLexicon :: S.Name -> FragmentCtx (Maybe S.Expr)
checkLexicon name = asks (Map.lookup name)

-- | Construct an expression tree from a constituency tree, pulling lexical
--   entries from the fragment.
mkExprTree :: Frag.Fragment -> ConstituencyTree -> ExprTree
mkExprTree frag cTree = runReader (mk cTree) frag
  where
    mk :: ConstituencyTree -> FragmentCtx ExprTree
    mk (T.Node cl@(T.CLabel (T.LexLabel lexLabel) _) _ _) = do
      eLabel <- checkLexicon lexLabel
      pure $ T.Node (eLabel, cl) T.Leaf T.Leaf
    mk (T.Node cl c0 c1) = case (c0, c1) of
      (_, T.Leaf) -> preTerm cl c0
      (T.Leaf, _) -> preTerm cl c1
      _ -> do
        e0 <- mk c0
        e1 <- mk c1
        pure $ T.Node (Nothing, cl) e0 e1
    preTerm :: ConstituencyLabel -> ConstituencyTree -> FragmentCtx ExprTree
    preTerm cl@(T.CLabel (T.CatLabel cLabelPre) _) term@(T.Node (T.CLabel (T.LexLabel cLabelTerm) _) _ _) = do
      termNode <- mk term

      let mkPreTermNode label = pure $ T.Node (label, cl) T.Leaf termNode

      case termNode of
        T.Node (Just termExpr, _) _ _ -> mkPreTermNode $ Just termExpr
        _ -> do
          eLabelPre <- checkLexicon cLabelPre
          case eLabelPre of
            Just preExpr -> mkPreTermNode $ Just $ S.rename cLabelPre (titleCase cLabelTerm) preExpr
            Nothing -> mkPreTermNode Nothing

-- | Type an expression tree, passing down binder assignments.
typeCheckExprTree :: ExprTree -> (TypeCheckedExprTree, TyEnv.Env)
typeCheckExprTree eTree = runState (mapM typeCheck eTree) TyEnv.empty
  where
    updateTypeEnv expr gTy = \env -> case expr of
      (S.EBinder (S.Binder n t)) -> TyEnv.extend env (n, gTy)
      _ -> env

    typeCheck :: ExprLabel -> State TyEnv.Env TypeCheckedExprLabel
    typeCheck (eLabel, cLabel) = case eLabel of
      Nothing -> pure (Nothing, cLabel)
      Just expr -> do
        env <- get
        case Inf.inferExpr env expr of
          Left err -> pure (Just $ UntypedExpr expr err, cLabel)
          Right ty -> case ty of
            gTy@(S.Forall _ iTy) -> do
              modify (updateTypeEnv expr gTy)
              pure $ (Just $ TypedExpr expr iTy, cLabel)

pattern FnNode e tDom <- TypedExpr e (S.TyFun tDom _)

pattern ArgNode e t <- TypedExpr e t

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

pattern TypedNode e <- T.Node (Just e@TypedExpr {}, _) _ _

composeExprTree :: (TypeCheckedExprTree, TyEnv.Env) -> TypeCheckedExprTree
composeExprTree (leaf@T.Leaf, _) = leaf
composeExprTree (node@(T.Node te c0 c1), env) = foldl' tryOp node' ops
  where
    c0' = composeExprTree (c0, env)
    c1' = composeExprTree (c1, env)
    node' = T.Node te c0' c1'
    ops = [functionApplication, predicateAbstraction, predicateModification]
    tryOp node@(T.Node (_, cl) c0 c1) op =
      let mkNode expr = T.Node (Just expr, cl) c0 c1
       in case (c0, c1) of
            (TypedNode e0, TypedNode e1) -> case op e0 e1 of
              Just expr -> case Inf.inferExpr env expr of
                Right (S.Forall _ ty) -> mkNode $ TypedExpr expr ty
                Left err -> mkNode $ UntypedExpr expr err
              _ -> node
            _ -> node

-- | Evaluate an expression tree, passing bound variables down.
evalExprTree :: TypeCheckedExprTree -> SemanticTree
evalExprTree eTree = evalState (mapM eval eTree) Sem.emptyCtx
  where
    updateCtx v = \ctx -> case v of
      Sem.VBinder (S.Binder n _) -> Sem.extendCtx n (Sem.VFormula $ S.Var n) ctx
      _ -> ctx
    eval :: TypeCheckedExprLabel -> State Sem.EvalCtx SemanticLabel
    eval (eLabel, cLabel) = case eLabel of
      Just expr -> case expr of
        UntypedExpr {} -> pure (Nothing, eLabel, cLabel)
        TypedExpr expr ty -> do
          ctx <- get
          case Sem.runEvalIn ctx expr of
            Right v -> do
              modify $ updateCtx v
              pure (Just $ Right v, eLabel, cLabel)
            Left err -> pure (Just $ Left err, eLabel, cLabel)
      Nothing -> pure (Nothing, Nothing, cLabel)

compose :: Frag.Fragment -> ConstituencyTree -> SemanticTree
compose f c = (evalExprTree . composeExprTree . typeCheckExprTree) (mkExprTree f c)