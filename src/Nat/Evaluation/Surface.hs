{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Evaluation.Surface where

import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Fixed (mod')
import Data.Foldable (Foldable (foldl', foldr'), length)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree.Binary.Preorder (Tree)
import Debug.Trace (trace, traceM)
import GHC.Real (Integral (quotRem))
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
  | NotEnumerable Expr
  deriving (Eq, Show)

type RelationEnv = Map.Map Type (Map.Map BinOp Expr)

data ExprReductionEnv = ExprRedEnv {tyEnv :: TypeEnv, relEnv :: RelationEnv}

type ExprReductionT r = Reduction r ExprEvalError ExprReductionEnv

bool :: Expr -> Bool
bool e = case e of
  (ELit (LBool b)) -> b
  _ -> error ("can only extract bool from literal bool: " ++ show e)

isIdx e = case e of EIdx {} -> True; _ -> False

dIdx (EIdx i) = i
dIdx e = error ("not an index: " ++ show e)

reducible expr = case expr of EVar {} -> False; ELit {} -> False; _ -> True

instance Reducible (Set Expr) (Set Expr) ExprEvalError ExprReductionEnv where
  reduce s = do
    s' <- mapM reduce (Set.toList s)
    pure $ Set.fromList s'

instance Reducible (QRstr Expr) (QRstr (Set Expr)) ExprEvalError ExprReductionEnv where
  reduce (QRstr v e) = do
    e' <- reduce e
    QRstr v <$> case e' of
      ESet es -> pure es
      _ -> throwError $ NotEnumerable e

instance Reducible (QExpr Expr) Expr ExprEvalError ExprReductionEnv where
  reduce q = do
    t <- case q of
      Univ rs b -> test allM' rs b
      Exis rs b -> test anyM' rs b
    -- Iota rs b ->
    pure $ ELit $ LBool t
    where
      split = unzip . fmap (\(QRstr v es) -> (v, Set.toList es))
      combine (vs, ds) = fmap (zip vs) (sequenceA ds)
      envs = combine . split

      test :: ([Expr -> ExprReductionT Bool] -> (Expr -> ExprReductionT Bool)) -> [QRstr Expr] -> Expr -> ExprReductionT Bool
      test q rs b = do
        rs' <- mapM (reduce :: QRstr Expr -> ExprReductionT (QRstr (Set Expr))) rs
        q [pure . bool <=< reduce . inEnv' env | env <- envs rs'] b

desugar = walk $ \case
  EInv f as -> foldl' EApp f as
  EFun bs e -> foldr' ELam e bs
  e -> e

primUnOpMap :: UnOp -> Expr -> Expr
primUnOpMap op = case op of
  Neg -> ELit . LBool . not . bool
  Len -> \case
    ESet s -> ELit (LInt (fromIntegral $ length s))
    e -> unexpectedOperand op e
  where
    unexpectedOperand op e = error ("unexpected operand " ++ show e ++ " for op " ++ show op)

primBiOpMap :: BinOp -> Expr -> Expr -> Expr
primBiOpMap op = case op of
  Eq -> l (==) Eq
  NEq -> l (/=) NEq
  LT -> o (<)
  LTE -> o (<=)
  GT -> o (P.>)
  GTE -> o (>=)
  Add -> i (+)
  Sub -> i (-)
  Mul -> i (P.*)
  Mod -> i mod'
  Impl -> b (\e e' -> e' || not e)
  Mem -> \e0 e1 -> case (e0, e1) of
    (e, ESet s) -> ELit (LBool (Set.member e s))
    _ -> unexpectedOperands op e0 e1
  -- overloaded ops
  _ -> \e0 e1 -> case (e0, e1) of
    (ESet {}, ESet {}) -> case op of
      Div -> s Set.difference e0 e1
      And -> s Set.union e0 e1
      Or -> s Set.intersection e0 e1
    (ELit {}, ELit {}) -> case op of
      Div -> i (/) e0 e1
      And -> b (&&) e0 e1
      Or -> b (||) e0 e1
    _ -> unexpectedOperands op e0 e1
  where
    unexpectedOperands op e0 e1 = error ("unexpected operands " ++ show e0 ++ " " ++ show e1 ++ " for op " ++ show op)
    l f _ (ELit l0) (ELit l1) = ELit $ LBool (f l0 l1)
    l _ op e0 e1 = EBinOp op e0 e1
    o f (ELit (LInt i0)) (ELit (LInt i1)) = ELit $ LBool (f i0 i1)
    o _ _ _ = error "ordinal expressions only"
    b f (ELit (LBool b0)) (ELit (LBool b1)) = ELit $ LBool (f b0 b1)
    b _ _ _ = error "boolean expressions only"
    i f (ELit (LInt i0)) (ELit (LInt i1)) = ELit $ LInt (f i0 i1)
    i _ e0 e1 = error (show op ++ " accepts integer expressions only, got: " ++ show e0' ++ ", " ++ show e1')
      where
        Right e0' = runReduce e0 :: Either ExprEvalError Expr
        Right e1' = runReduce e1 :: Either ExprEvalError Expr
    s f (ESet s0) (ESet s1) = ESet (f s0 s1)
    s _ _ _ = error "set expressions only"
    sb f (ESet s0) (ESet s1) = ELit (LBool (f s0 s1))
    sb _ _ _ = error "set expressions only"
    si f (ESet s) = ELit (LInt (f s))
    si _ _ = error "set expressions only"

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
    EUnOp op e -> do
      e' <- reduce e
      pure $ primUnOpMap op e'
    EBinOp op e0 e1 -> do
      e0' <- reduce e0
      e1' <- reduce e1

      eArgTy <- tyOf e0' -- the type checker enforces equality of e0 & e1
      relEnv <- asks relEnv

      let primOp = let op' = primBiOpMap op in pure $ e0' `op'` e1'
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
    ETree t -> reduce $ mkChurchTree t
    EFix v e -> reduce $ mkFixPoint v e
    ETup es -> ETup <$> mapM reduce es
    EQnt q -> reduce q
    -- (3) var/literal
    _ -> pure expr

  -- reduction to normal form
  normalize expr =
    reduce expr >>= \case
      ELam b body -> do
        body' <- norm body
        pure (ELam b body')
      EApp e0 e1 -> do
        e0' <- norm e0
        e1' <- norm e1
        pure (e0 * e1)
      EBinOp op e0 e1 -> do
        e0' <- norm e0
        e1' <- norm e1
        pure $ EBinOp op e0' e1'
      expr' -> pure expr'
    where
      norm :: Expr -> ExprReductionT Expr
      norm = normalize

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