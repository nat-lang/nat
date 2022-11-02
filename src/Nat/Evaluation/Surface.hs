{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Evaluation.Surface where

import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Foldable (Foldable (foldl', foldr'))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tree.Binary.Preorder (Tree)
import Debug.Trace (traceM)
import Nat.Context
import Nat.Control
import Nat.Evaluation.Context
import Nat.Evaluation.Type hiding (fresh, mkChurchTree, normalize)
import Nat.Inference
import Nat.Reduction
import Nat.Syntax.Surface
import Nat.Syntax.Type
import Nat.Unification
import Nat.Viz
import Nat.Walk
import Prelude hiding (GT, LT, (*), (>))
import qualified Prelude as P

data ExprEvalError
  = InexhaustiveCase Expr
  | EUnificationError (UnificationError Expr)
  | RuntimeTypeError (InferenceError Type Expr)
  | CompilationTypeError (InferenceError Type Expr)
  deriving (Eq, Show)

type RelationEnv = Map.Map Type (Map.Map BinOp Expr)

data ExprReductionEnv = ExprRedEnv {tyEnv :: TypeEnv, relEnv :: RelationEnv}

bool :: Expr -> Bool
bool e = case e of
  (ELit (LBool b)) -> b
  _ -> error ("can only extract bool from literal bool: " ++ show e)

numErr = error "bool is not a num"

instance Num Lit where
  (+) (LInt i0) (LInt i1) = LInt (i0 + i1)
  (*) (LInt i0) (LInt i1) = LInt (i0 P.* i1)
  abs (LInt i) = LInt (abs i)
  signum (LInt l) = LInt (signum l)
  fromInteger i = LInt (fromIntegral i)
  negate (LInt i) = LInt (negate i)

isIdx e = case e of EIdx {} -> True; _ -> False

dIdx (EIdx i) = i
dIdx _ = error "not an index"

reducible expr = case expr of EVar {} -> False; ELit {} -> False; _ -> True

instance Reducible (Set.Set Expr) (Set.Set Expr) ExprEvalError ExprReductionEnv where
  reduce s = do
    s' <- mapM reduce (Set.toList s)
    pure $ Set.fromList s'

instance Reducible (QExpr Expr) Expr ExprEvalError ExprReductionEnv where
  reduce q = do
    t <- case q of
      Univ rs b -> allM' (test rs) b
      Exis rs b -> anyM' (test rs) b
    pure $ ELit $ LBool t
    where
      test rs =
        let (vars, doms) = unzip $ fmap (\(QRstr v (Dom _ es)) -> (v, Set.toList es)) rs
            envs = fmap (zip vars) (sequenceA doms)
         in [pure . bool <=< reduce . inEnv' env | env <- envs]

desugar = walk $ \case
  ETree t -> mkChurchTree t
  EInv f as -> foldl' EApp f as
  EFun bs e -> foldr' ELam e bs
  EQnt q -> desugar $ case q of
    Univ rs b -> join And rs b
    Exis rs b -> join Or rs b
    where
      join op rs b = foldl1 (EBinOp op) (bindEnum rs b)
      qrBind (QRstr v (Dom t _)) = Binder v t
      qrDom (QRstr _ (Dom _ s)) = Set.toList s
      bindEnum rs e =
        let binds = fmap qrBind rs
            args = traverse qrDom rs
         in [EInv (EFun binds e) a | a <- args]
  e -> e

primOpMap :: BinOp -> Expr -> Expr -> Expr
primOpMap op = case op of
  Eq -> l (==) Eq
  NEq -> l (/=) NEq
  LT -> o (<)
  LTE -> o (<=)
  GT -> o (P.>)
  GTE -> o (>=)
  And -> b (&&)
  Or -> b (||)
  Impl -> b (\e e' -> e' || not e)
  Add -> i (+)
  Sub -> i (-)
  Mul -> i (P.*)
  where
    l f _ (ELit l0) (ELit l1) = ELit $ LBool (f l0 l1)
    l _ op e0 e1 = EBinOp op e0 e1
    o f (ELit (LInt i0)) (ELit (LInt i1)) = ELit $ LBool (f i0 i1)
    o _ _ _ = error "ordinal expressions only"
    b f (ELit (LBool b0)) (ELit (LBool b1)) = ELit $ LBool (f b0 b1)
    b _ _ _ = error "boolean expressions only"
    i f (ELit (LInt i0)) (ELit (LInt i1)) = ELit $ LInt (f i0 i1)
    i _ _ _ = error "integer expressions only"

mkReductionEnv = ExprRedEnv {tyEnv = mkCEnv, relEnv = Map.empty}

tyOf e = asks (\s -> inferIn (tyEnv s) e)

tryTyOf e = do
  eTy <- tyOf e
  case eTy of
    Left err -> throwError $ RuntimeTypeError err
    Right t -> pure t

instance Reducible Expr Expr ExprEvalError ExprReductionEnv where
  runReduce = runReduce' mkReductionEnv
  runNormalize = runNormalize' mkReductionEnv

  -- cbn reduction to weak head normal form
  reduce expr = case expr of
    -- (1) leftmost, outermost
    EApp e0 e1 -> case e0 of
      -- (1a) function application, beta reduce
      ELam (Binder v _) body -> reduce (sub v e1 body)
      -- (sugar) combinatorially, sets behave like their characteristic functions
      ESet s -> do
        e1' <- reduce e1
        pure $ ELit $ LBool $ Set.member e1' s
      -- (sugar) indexed access to tuples
      ETup t | isIdx e1 -> reduce (t !! dIdx e1)
      -- (1b) binders have a semantics of their own: they may be applied
      -- to terms, in which case they simply abstract a free variable.
      EBind b -> reduce (ELam b e1)
      -- (1c) normal form for lhs, goto rhs
      -- here we allow unbound variables into the normal form
      -- as unreduced redexes. this also allows literals, but
      -- that doesn't type check.
      _ | not (reducible e0) -> do
        e1' <- reduce e1
        pure (e0 * e1')
      -- (1d) lhs can be reduced
      _ -> do
        e0' <- reduce e0
        case e0' of
          --  (1d.1) here we encounter (1c)
          -- normal form for lhs, goto rhs
          EApp {} -> do
            e1' <- reduce e1
            pure (e0' * e1')
          -- (1d.2) otherwise goto top
          _ -> reduce (e0' * e1)
    -- (2) sugar
    EUnOp Neg e -> do
      e' <- reduce e
      pure $ ELit $ LBool $ not $ bool e'
    EBinOp op e0 e1 -> do
      e0' <- reduce e0
      e1' <- reduce e1

      eArgTy <- tyOf e0' -- the type checker enforces equality of e0 & e1
      relEnv <- asks relEnv

      let primOp = let op' = primOpMap op in pure $ e0' `op'` e1'
          uOp argTy = case Map.lookup argTy relEnv of
            Nothing -> primOp
            Just relMap -> maybe primOp (\op -> reduce (op * e0 * e1)) (Map.lookup op relMap)

      either (const primOp) uOp eArgTy
    ECond x y z -> do
      x' <- reduce x
      case x' of
        ELit LBool {} -> reduce $ if bool x' then y else z
        _ -> pure $ ECond x' y z
    ETyCase b cs -> do
      b' <- reduce b
      bTy <- tryTyOf b'

      case cs of
        ((Binder p pTy, e) : cs') ->
          if unifiable bTy pTy
            then case runUnify [(p, b')] of
              Left e -> throwError $ EUnificationError e
              Right s -> reduce $ inEnv s e
            else reduce $ ETyCase b cs'
        _ -> throwError $ InexhaustiveCase expr
    ELitCase b cs -> do
      b' <- reduce b
      case cs of
        ((c, e) : cs') -> do
          c' <- reduce c
          e' <- reduce e
          reduce $ (b' === c') ? e' > ELitCase b cs'
        _ -> throwError $ InexhaustiveCase expr
    EFix v e -> reduce $ mkFixPoint v e
    ETup es -> ETup <$> mapM reduce es
    EQnt q -> reduce q
    -- (3) var/literal
    _ -> pure expr

  -- reduction to normal form
  normalize expr =
    reduce expr >>= \case
      ELam b body -> do
        body' <- normalize body
        pure (ELam b body')
      EApp e0 e1 -> do
        e0' <- normalize e0
        e1' <- normalize e1
        pure (e0 * e1)
      EBinOp op e0 e1 -> do
        e0' <- normalize e0
        e1' <- normalize e1
        pure $ EBinOp op e0' e1'
      expr' -> pure expr'

-- | We pattern match via unification.
instance Unifiable Expr where
  unify e0 e1 = case (e0, e1) of
    (EVar v0, EVar v1) | v0 == v1 -> pure mempty
    (ETup t0, ETup t1) | length t0 == length t1 -> unifyMany (zip t0 t1)
    (EUndef, EUndef) -> pure mempty
    (EVar v, _) -> pure $ mkEnv v e1
    (_, EVar v) -> pure $ mkEnv v e0
    (EWild, _) -> pure mempty
    (_, EWild) -> pure mempty
    _ -> throwError $ NotUnifiable e0 e1

type Normalization = Either ExprEvalError Expr

confluent :: Expr -> Expr -> Bool
confluent e0 e1 = case (reduce e0, reduce e1) of
  (Right e1', Right e0') -> e0' @= e1'
  _ -> False
  where
    reduce :: Expr -> Normalization
    reduce = runNormalize . desugar

e0 *= e1 = confluent e0 e1

e0 *!= e1 = not (e0 *= e1)

eval :: Expr -> Either ExprEvalError Expr
eval expr = case runSignify expr' of
  Left err -> Left $ CompilationTypeError err
  Right env -> runReduce' (ExprRedEnv {tyEnv = env, relEnv = Map.empty}) expr'
  where
    expr' = evalRename $ desugar expr