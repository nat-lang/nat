{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Surface where

import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tree.Binary.Preorder (Tree)
import Debug.Trace (trace, traceM)
import Mean.Evaluation.Context
import Mean.Evaluation.Type hiding (fresh, normalize)
import Mean.Inference
import Mean.Reduction
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Mean.Unification
import Mean.Var
import Mean.Viz
import Prelude hiding (GT, LT, (&&), (*), (+), (-), (>), (||))
import qualified Prelude as P

-- Equality up to renaming of free variables.
class AlphaEq a where
  alphaEq :: a -> a -> Bool

-- Evaluative equality is the kind we recognize during evaluation.
class EvalEq a where
  (=*=) :: a -> a -> Bool

data ExprEvalError
  = InexhaustiveCase Expr
  | EUnificationError (UnificationError Expr)
  | RuntimeTypeError (InferenceError Type Expr)
  deriving (P.Eq, Show)

incrVarId :: Var -> Var
incrVarId (Var vPub vPri) = Var vPub $ init vPri ++ show (digitToInt (last vPri) P.+ 1)

fresh :: Var -> Var -> Set.Set Var -> Var
fresh v0 v1 fv =
  let v0' = incrVarId v0
   in if v0' == v1 P.|| Set.member v0' fv
        then fresh v0' v1 fv
        else v0'

bool :: Expr -> Bool
bool e = case e of
  (ELit (LBool b)) -> b
  _ -> error "can only extract bool from literal bool"

class Arithmetic a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  mul :: a -> a -> a

arithOnlyErr = "can only perform arithmetic on natural numbers"

instance Arithmetic Expr where
  (ELit (LInt l0)) + (ELit (LInt l1)) = ELit $ LInt $ l0 P.+ l1
  _ + _ = error arithOnlyErr
  (ELit (LInt l0)) `mul` (ELit (LInt l1)) = ELit $ LInt $ l0 P.* l1
  _ `mul` _ = error arithOnlyErr
  (ELit (LInt l0)) - (ELit (LInt l1)) = ELit $ LInt $ l0 P.- l1
  _ - _ = error arithOnlyErr

-- | There are two possible conflicts for a substitution e'[e/v]:
--  (1) a nested binder conflicts with v, as in
--        (λf . f)[x/f]
--     in which case we want (λf . f) rather than (λf . x)
--  (2) a nested binder conflicts with a free variable in e, as in
--        (λf . f n)[f/n]
--     in which case we want (λf1 . f1 f) rather than (λf . f f)
instance Substitutable Expr Expr where
  substitute env e = foldl' (uncurry . sub) e (Map.toList env)
    where
      -- e'[e/v]
      sub :: Expr -> Var -> Expr -> Expr
      sub e' v e =
        let sub' e' = sub e' v e
         in case e' of
              -- relevant base case
              EVar v' | v' == v -> e
              -- induction
              EApp e0 e1 -> sub' e0 * sub' e1
              ECond x y z -> sub' x ? sub' y > sub' z
              EUnOp op e -> EUnOp op (sub' e)
              EBinOp op e0 e1 -> EBinOp op (sub' e0) (sub' e1)
              ETyCase b cs ->
                let (ts, es) = unzip cs
                 in ETyCase (sub' b) (zip ts (sub' <$> es))
              ETree t -> ETree $ fromList $ sub' <$> toList t
              -- ELitCase Expr [(Expr, Expr)]
              -- ESet (Set Expr)
              -- ELet Var Expr Expr
              -- EFix Var Expr
              -- induction, but rename binder if it conflicts with fv(e).
              ELam b@(Binder v' t) body
                | v /= v' ->
                  let fvE = fv e
                      fvB = fv body
                   in if v' `Set.member` fvE
                        then
                          let v'' = fresh v' v (fvE `Set.union` fvB)
                              body' = sub' $ sub body v' (EVar v'')
                           in ELam (Binder v'' t) body'
                        else ELam b (sub' body)
              -- irrelevent base cases
              _ -> e'

isIdx e = case e of EIdx {} -> True; _ -> False

idxOf (EIdx i) = i
idxOf _ = error "not an index"

reducible expr = case expr of EVar {} -> False; ELit {} -> False; _ -> True

instance Reducible (Tree Expr) Expr ExprEvalError TypeEnv where
  reduce t = reduce $ mkChurchTree t

instance Reducible Expr Expr ExprEvalError TypeEnv where
  runReduce = runReduce' mkCEnv
  runNormalize = runNormalize' mkCEnv

  -- cbn
  reduce expr = case expr of
    -- (1) leftmost, outermost
    EApp e0 e1 -> case e0 of
      -- (1a) function application, beta reduce
      ELam (Binder v _) body -> reduce (inEnv v e1 body)
      -- (sugar) combinatorially, sets behave like their characteristic functions
      ESet s -> do
        e1' <- reduce e1
        pure $ ELit $ LBool $ Set.member e1' s
      -- (sugar) indexed access to tuples
      ETup t | isIdx e1 -> reduce (t !! idxOf e1)
      -- (1b) binders have a semantics of their own: they may be applied
      -- to terms, in which case they simply abstract a free variable.
      EBind b -> reduce (ELam b e1)
      -- (1c) normal form for lhs, goto rhs
      _ | not (reducible e0) -> do
        e1' <- reduce e1
        pure (e0 * e1')
      -- (1d) lhs can be reduced
      _ -> do
        e0' <- reduce e0
        case e0' of
          -- (1d.1) normal form for lhs (app), goto rhs
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
      case infer b' of
        Left e -> throwError $ RuntimeTypeError e
        Right ty -> case cs of
          ((Binder v ty', e) : cs') ->
            if unifiable ty ty'
              then case runUnify [(v, b)] of
                Left e -> throwError $ EUnificationError e
                Right s -> reduce $ substitute s e
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
    ELet v e0 e1 -> reduce (inEnv v e0 e1)
    EFix v e -> reduce $ mkFixPoint v e
    ETup es -> ETup <$> mapM reduce es
    -- (3) var/literal
    _ -> pure expr

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

-- (x,y) (1,2)
instance Unifiable Expr where
  unify e0 e1 = case (e0, e1) of
    (EVar v0, EVar v1) | v0 == v1 -> pure mempty
    (EVar v, _) -> pure $ mkEnv v e1
    (_, EVar v) -> pure $ mkEnv v e0
    (ETup t0, ETup t1) | length t0 == length t1 -> unifyMany (zip t0 t1)
    _ -> throwError $ NotUnifiable e0 e1

instance AlphaEq [Expr] where
  alphaEq [] [] = True
  alphaEq (x0 : xs0) (x1 : xs1) = (x0 `alphaEq` x1) P.&& (xs0 `alphaEq` xs1)
  alphaEq _ _ = False

-- assumes e0 and e1 are in normal form
instance AlphaEq Expr where
  alphaEq e0 e1 = case (e0, e1) of
    (ELam (Binder v0 _) body0, ELam (Binder v1 _) body1) ->
      inEnv v0 (EVar v1) body0 @= body1 P.|| inEnv v1 (EVar v0) body1 @= body0
    (EApp e0a e0b, EApp e1a e1b) -> e0a @= e1a P.&& e0b @= e1b
    (EVar v0, EVar v1) -> v0 == v1
    (ELit l0, ELit l1) -> l0 == l1
    (EBinOp op e0a e1a, EBinOp op' e0b e1b) -> (op == op') P.&& (e0a @= e0b) P.&& (e1a @= e1b)
    (ECond x0 y0 z0, ECond x1 y1 z1) -> (x0 @= x1) P.&& (y0 @= y1) P.&& (z0 @= z1)
    (ESet s0, ESet s1) -> Set.toAscList s0 `alphaEq` Set.toAscList s1
    _ -> False

(@=) = alphaEq

(@!=) :: Expr -> Expr -> Bool
e0 @!= e1 = not (e0 @= e1)

eval :: Expr -> Either ExprEvalError Expr
eval = runReduce

type Normalization = Either ExprEvalError Expr

confluent :: Expr -> Expr -> Bool
confluent e0 e1 = case (runNormalize e0 :: Normalization, runNormalize e1 :: Normalization) of
  (Right e1', Right e0') -> e0' @= e1'
  _ -> False

e0 *= e1 = confluent e0 e1

e0 *!= e1 = not (e0 *= e1)
