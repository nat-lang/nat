{-# LANGUAGE PatternSynonyms #-}

module Interpreter.Compose (
  ExprTree, ExprLabel(..),
  TypeCheckedExpr(..), TypeCheckedExprLabel,
  SemanticTree, SemanticLabel(..), EvaluatedExpr(..),
  compose
  ) where

import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Utils
import Compiler.Core.Pretty
import qualified Compiler.Core.Syntax as S
import qualified Compiler.Core.Inference as Inf
import qualified Compiler.Core.TypeEnv as TyEnv
import Compiler.Tree.Syntax as T
import qualified Interpreter.Fragment as Frag
import qualified Interpreter.Evaluation as Sem

import Debug.Trace (traceM)

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
    mk (T.Node cl c0 c1) = case (c0,c1) of
      (_, T.Leaf) -> preTerm cl c0
      (T.Leaf, _) -> preTerm cl c1
      _ -> do
        e0 <- mk c0
        e1 <- mk c1
        pure $ T.Node (Nothing, cl) e0 e1

    -- | Given a preterminal φ and terminal ψ, if ψ has a lexical entry then
    --   assign it to φ, otherwise if φ has a lexical entry, instantiate it with ψ.
    --   In the latter case, if φ is of the form `[C] = \x . C x`, where C is a
    --   syntactic constant, rename C to ψ in φ.
    preTerm :: ConstituencyLabel -> ConstituencyTree -> FragmentCtx ExprTree
    preTerm cl@(T.CLabel (T.CatLabel cLabelPre ) _) term@(T.Node (T.CLabel (T.LexLabel cLabelTerm) _) _ _) = do
      termNode <- mk term

      let mkPreTermNode label = pure $ T.Node (label, cl) T.Leaf termNode

      case termNode of
        T.Node (Just termExpr, _) _ _ -> mkPreTermNode $ Just termExpr
        _ -> do
          eLabelPre <- checkLexicon cLabelPre
          case eLabelPre of
            Just preExpr -> mkPreTermNode $ Just $ S.rename cLabelPre (titleCase cLabelTerm) preExpr
            Nothing      -> mkPreTermNode Nothing

-- | Type an expression tree, passing down binder assignments.
typeCheckExprTree :: ExprTree -> (TypeCheckedExprTree, TyEnv.Env)
typeCheckExprTree eTree = runState (mapM typeCheck eTree) TyEnv.empty
  where
    updateTypeEnv expr gTy = \env -> case expr of
      (S.EBinder (S.Binder n t)) -> TyEnv.extend env (n, gTy)
      _ -> env

    typeCheck :: ExprLabel -> State TyEnv.Env TypeCheckedExprLabel
    typeCheck (eLabel, cLabel) = case eLabel of
      Nothing -> pure $ (Nothing, cLabel)
      Just expr -> do
        env <- get
        case Inf.inferExpr env expr of
          Left err -> pure $ (Just $ UntypedExpr expr err, cLabel)
          Right ty -> case ty of
            gTy@(S.Forall _ iTy) -> do
              modify (updateTypeEnv expr gTy)
              pure $ (Just $ TypedExpr expr iTy, cLabel)

pattern LegalFnNode tDom tRan e <- T.Node (Just (TypedExpr e (S.TyFun tDom tRan)), cl) _ _
pattern LegalArgNode t e <- T.Node (Just (TypedExpr e t), cl) _ _
pattern LegalBindNode b <- T.Node (Just (TypedExpr (S.EBinder b) t), cl) _ _

tryCompositionOps :: TyEnv.Env -> ConstituencyLabel -> TypeCheckedExprTree -> TypeCheckedExprTree -> TypeCheckedExprTree
tryCompositionOps env cl c0 c1 = case c0 of
  LegalBindNode b -> mkBindNode b c1
  LegalFnNode t1Dom t1Ran e0 -> case c1 of
    LegalBindNode b -> mkBindNode b c0
    LegalArgNode t2 e1 | Inf.unifiable t1Dom t2 -> mkAppNode e0 e1
    _ -> fallthru
  LegalArgNode t1 e0 -> case c1 of
    LegalBindNode b -> mkBindNode b c0
    LegalFnNode t2Dom t2Ran e1 | Inf.unifiable t2Dom t1 -> mkAppNode e1 e0
    _ -> fallthru
  _ -> fallthru
  where
    mkBindNode :: S.Binder -> TypeCheckedExprTree -> TypeCheckedExprTree
    mkBindNode b@(S.Binder _ t0) (T.Node (tELabel, _) _ _) = case tELabel of
      Just (TypedExpr e t1) -> compose $ TypedExpr (S.Lam b e) (S.TyFun t0 t1)
      _ -> fallthru

    mkAppNode :: S.Expr -> S.Expr -> TypeCheckedExprTree
    mkAppNode e0 e1 = let e = (S.App e0 e1)
      in
        case Inf.inferExpr env e of
          Right (S.Forall _ ty) -> compose $ TypedExpr e ty
          Left err -> compose $ UntypedExpr e err

    compose label = T.Node (Just label, cl) c0 c1
    fallthru = T.Node (Nothing, cl) c0 c1

-- | Construct non-terminal expressions via our composition operations.
composeExprTree :: (TypeCheckedExprTree, TyEnv.Env) -> TypeCheckedExprTree
composeExprTree (n@(T.Node (_,cnl) c1 c2), env) = case (c1,c2) of
  (T.Node{}, T.Node{}) -> let
    s1 = composeExprTree (c1, env)
    s2 = composeExprTree (c2, env)
    in tryCompositionOps env cnl s1 s2
  _ -> n

type ExprTreeEval = StateT Sem.EvalCtx Identity SemanticTree

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
        UntypedExpr{} -> pure $ (Nothing, eLabel, cLabel)
        TypedExpr expr ty -> do
          ctx <- get
          case (Sem.runEvalIn ctx expr) of
            Right v -> do
              modify $ updateCtx v
              pure $ (Just $ Right v, eLabel, cLabel)
            Left err -> pure $ (Just $ Left err, eLabel, cLabel)
      Nothing -> pure $ (Nothing, Nothing, cLabel)

compose :: Frag.Fragment -> ConstituencyTree -> SemanticTree
compose f c = (evalExprTree . composeExprTree . typeCheckExprTree) (mkExprTree f c)