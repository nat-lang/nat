{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Surface where

import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import qualified Data.Set as Set
import Data.Tree.Binary.Preorder (Tree)
import Mean.Context
import Mean.Control
import Mean.Evaluation.Context
import Mean.Evaluation.Type hiding (fresh, mkChurchTree, normalize)
import Mean.Inference
import Mean.Reduction
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Mean.Unification
import Mean.Viz
import Prelude hiding (GT, LT, (&&), (*), (+), (-), (>), (||))
import qualified Prelude as P

data ExprEvalError
  = InexhaustiveCase Expr
  | EUnificationError (UnificationError Expr)
  | RuntimeTypeError (InferenceError Type Expr)
  | CompilationTypeError (InferenceError Type Expr)
  deriving (P.Eq, Show)

bool :: Expr -> Bool
bool e = case e of
  (ELit (LBool b)) -> b
  _ -> error ("can only extract bool from literal bool: " ++ show e)

class Arithmetic a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  mul :: a -> a -> a

  arithOnlyErr :: a
  arithOnlyErr = error "can only perform arithmetic on natural numbers"

instance Arithmetic Expr where
  (ELit (LInt l0)) + (ELit (LInt l1)) = ELit $ LInt $ l0 P.+ l1
  _ + _ = arithOnlyErr
  (ELit (LInt l0)) `mul` (ELit (LInt l1)) = ELit $ LInt $ l0 P.* l1
  _ `mul` _ = arithOnlyErr
  (ELit (LInt l0)) - (ELit (LInt l1)) = ELit $ LInt $ l0 P.- l1
  _ - _ = arithOnlyErr

isIdx e = case e of EIdx {} -> True; _ -> False

dIdx (EIdx i) = i
dIdx _ = error "not an index"

reducible expr = case expr of EVar {} -> False; ELit {} -> False; _ -> True

instance Reducible (Tree Expr) Expr ExprEvalError TypeEnv where
  -- TODO: instantiate the tree with fresh variable names.
  -- currently we avoid shadowing by using a different format for
  -- private church tree vars (e.g. no postfixed numbers),
  -- but this only works if no two trees are in the same scope.
  reduce = reduce . mkChurchTree

instance Reducible (Set.Set Expr) (Set.Set Expr) ExprEvalError TypeEnv where
  reduce s = do
    s' <- mapM reduce (Set.toList s)
    pure $ Set.fromList s'

instance Reducible (QExpr Expr) Expr ExprEvalError TypeEnv where
  reduce q = do
    t <- case q of
      Univ rs b -> allM (test rs) b
      Exis rs b -> anyM (test rs) b
    pure $ ELit $ LBool t
    where
      test rs =
        let (vars, doms) = unzip $ fmap (\(QRstr v (Dom _ es)) -> (v, Set.toList es)) rs
            envs = fmap (zip vars) (sequenceA doms)
         in [pure . bool <=< reduce . inEnv' env | env <- envs]

instance Reducible Expr Expr ExprEvalError TypeEnv where
  runReduce = runReduce' mkCEnv
  runNormalize = runNormalize' mkCEnv

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
      -- here we allow unbound (so unreducible) variables of tyfun into
      -- the normal form. this also allows literals, but that doesn't type check.
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
    EBinOp op e0 e1 ->
      let b = ELit . LBool
       in do
            e0' <- reduce e0
            e1' <- reduce e1
            pure $ case op of
              -- term level equality is selectively defined
              Eq ->
                let eq = b $ (==) e0' e1'
                    noop = EBinOp op e0' e1'
                 in case (e0', e1') of
                      (ELit {}, ELit {}) -> eq
                      (ESet {}, ESet {}) -> eq
                      _ -> noop
              -- as should inequality be
              NEq -> b $ (/=) e0' e1'
              LT -> b $ (<) e0' e1'
              LTE -> b $ (<=) e0' e1'
              GT -> b $ (P.>) e0' e1'
              GTE -> b $ (>=) e0' e1'
              And -> b $ and $ bool <$> [e0', e1']
              Or -> b $ or $ bool <$> [e0', e1']
              Impl -> b $ or [bool e1', not (bool e0')]
              Add -> (+) e0' e1'
              Sub -> (-) e0' e1'
              Mul -> mul e0' e1'
    ECond x y z -> do
      x' <- reduce x
      case x' of
        ELit LBool {} -> reduce $ if bool x' then y else z
        _ -> pure $ ECond x' y z
    ETree t -> reduce t
    ETyCase b cs -> do
      b' <- reduce b
      env <- ask
      case inferIn env b' of
        Left e -> throwError $ RuntimeTypeError e
        Right ty -> case cs of
          ((Binder p ty', e) : cs') ->
            if unifiable ty ty'
              then case runUnify [(p, b)] of
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
confluent e0 e1 = case (runNormalize e0 :: Normalization, runNormalize e1 :: Normalization) of
  (Right e1', Right e0') -> e0' @= e1'
  _ -> False

e0 *= e1 = confluent e0 e1

e0 *!= e1 = not (e0 *= e1)

eval :: Expr -> Either ExprEvalError Expr
eval expr = case runSignify expr' of
  Left err -> Left $ CompilationTypeError err
  Right env -> runReduce' env expr'
  where
    expr' = evalRename expr