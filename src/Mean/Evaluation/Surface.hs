{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Mean.Evaluation.Surface where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Char (digitToInt)
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Mean.Evaluation.Type (TypeError, infer, (<=>))
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Mean.Viz
import Prelude hiding (GT, LT, (&&), (*), (+), (-), (>), (||))
import qualified Prelude as P

type Evaluation = ExceptT EvalError Identity

class Reducible a where
  fv :: a -> Set.Set Name
  reduce :: a -> Evaluation Expr
  substitute :: Expr -> Var -> a -> a

class AlphaEq a where
  alphaEq :: a -> a -> Bool

-- Evaluative equality is the kind we recognize during evaluation.
class EvalEq a where
  (=*=) :: a -> a -> Bool

data EvalError
  = InexhaustiveCase Expr
  | RuntimeTypeError TypeError
  deriving (P.Eq)

instance Show EvalError where
  show (InexhaustiveCase c) = "Inexhaustive case expr: " ++ show c
  show (RuntimeTypeError e) = "Runtime type error: " ++ show e

incrVarId :: Var -> Var
incrVarId (Var vPub vPri) = Var vPub $ init vPri ++ show (digitToInt (last vPri) P.+ 1)

fresh :: Var -> Var -> Set.Set Name -> Var
fresh v0 v1 fv =
  let v0'@(Var _ v0'Pri) = incrVarId v0
   in if v0' == v1 P.|| Set.member v0'Pri fv
        then fresh v0' v1 fv
        else v0'

bool :: Expr -> Bool
bool e = case e of
  (ELit (LBool b)) -> b
  _ -> error "can only extract bool from literal bool"

instance Reducible [Expr] where
  fv es = foldl1 Set.union (fv <$> es)
  substitute e v es = substitute e v <$> es

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

-- λeb . e
churchLeaf =
  let [e, b] = mkEVar <$> ["e", "b"]
   in e ~> (b ~> e)

-- λxlreb . b(x)(l e b)(r e b)
churchNode =
  let [e, b, x, l, r] = mkEVar <$> ["e", "b", "x", "l", "r"]
   in x ~> (l ~> (r ~> (e ~> (b ~> (b * x * (l * e * b) * (r * e * b))))))

instance Reducible Expr where
  fv e = case e of
    EVar (Var _ v) -> Set.singleton v
    ELam (Binder (Var _ v) _) body -> fv body Set.\\ Set.singleton v
    EApp e0 e1 -> fv [e0, e1]
    ECond x y z -> fv [x, y, z]
    EUnOp _ e -> fv e
    EBinOp _ e0 e1 -> fv [e0, e1]
    -- ETree (T.Tree Expr)
    -- ELitCase Expr [(Expr, Expr)]
    -- ESet (Set Expr)
    -- ELet Var Expr Expr
    -- EFix Var Expr
    _ -> Set.empty

  -- e'[e/v]
  substitute e v e' =
    let sub' = substitute e v
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
             in ETyCase (sub' b) (zip ts (substitute e v es))
          -- ETree (T.Tree Expr)
          -- ELitCase Expr [(Expr, Expr)]
          -- ESet (Set Expr)
          -- ELet Var Expr Expr
          -- EFix Var Expr
          -- induction, but rename binder if it conflicts with fv(e)
          ELam b@(Binder v'@(Var _ v'Pri) t) body
            | v /= v' ->
              let fvE = fv e
                  fvB = fv body
               in if v'Pri `Set.member` fvE
                    then
                      let v'' = fresh v' v (fvE `Set.union` fvB)
                          body' = sub' $ substitute (EVar v'') v' body
                       in ELam (Binder v'' t) body'
                    else ELam b (sub' body)
          -- irrelevent base cases
          _ -> e'

  -- cbn
  reduce expr = case expr of
    -- (1) leftmost, outermost
    EApp e0 e1 -> case e0 of
      -- (1a) function application, beta reduce
      ELam (Binder v _) body -> reduce (substitute e1 v body)
      -- (sugar) combinatorially, sets behave like their characteristic functions
      ESet s -> do
        e1' <- reduce e1
        pure $ ELit $ LBool $ Set.member e1' s
      -- (1b) binders have a semantics of their own: they may be applied
      -- to terms, in which case they simply abstract a free variable.
      EBind b -> reduce (ELam b e1)
      -- (1c) normal form for lhs (free var), goto rhs
      EVar {} -> do
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
    ETree t -> mkChurchTree t >>= reduce
      where
        mkChurchTree t = case t of
          Leaf -> pure churchLeaf
          Node e l r -> do
            l' <- mkChurchTree l
            r' <- mkChurchTree r
            pure $ churchNode * e * l' * r'
    ETyCase b cs -> case infer b of
      Left e -> throwError $ RuntimeTypeError e
      Right (Forall _ ty) -> case cs of
        ((Binder v ty', e) : cs') ->
          reduce $
            if ty <=> ty'
              then substitute b v e
              else ETyCase b cs'
        _ -> throwError $ InexhaustiveCase expr
    ELitCase b cs -> do
      b' <- reduce b
      case cs of
        ((c, e) : cs') -> do
          c' <- reduce c
          e' <- reduce e
          reduce $ (b' === c') ? e' > ELitCase b cs'
        _ -> throwError $ InexhaustiveCase expr
    ELet v e0 e1 -> reduce $ (EVar v ~> e1) * e0
    EFix v e ->
      let f = mkEVar "f"
          x = mkEVar "x"
          y = f ~> ((x ~> (f * (x * x))) * (x ~> (f * (x * x))))
       in reduce (y * (EVar v ~> e))
    -- (3) var/literal
    _ -> pure expr

-- normal form
simplify :: Reducible a => a -> Evaluation Expr
simplify expr =
  reduce expr >>= \case
    ELam b body -> do
      body' <- simplify body
      pure (ELam b body')
    EApp e0 e1 -> do
      e0' <- simplify e0
      e1' <- simplify e1
      pure (e0 * e1)
    EBinOp op e0 e1 -> do
      e0' <- simplify e0
      e1' <- simplify e1
      pure $ EBinOp op e0' e1'
    expr' -> pure expr'

instance AlphaEq [Expr] where
  alphaEq [] [] = True
  alphaEq (x0 : xs0) (x1 : xs1) = (x0 `alphaEq` x1) P.&& (xs0 `alphaEq` xs1)
  alphaEq _ _ = False

-- assumes e0 and e1 are in normal form
instance AlphaEq Expr where
  alphaEq e0 e1 = case (e0, e1) of
    (ELam (Binder v0 _) body0, ELam (Binder v1 _) body1) ->
      substitute (EVar v1) v0 body0 @= body1 P.|| substitute (EVar v0) v1 body1 @= body0
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

eval :: Reducible a => a -> Either EvalError Expr
eval e = runIdentity $ runExceptT $ reduce e

eval' :: Reducible a => a -> Either EvalError Expr
eval' e = runIdentity $ runExceptT $ simplify e

confluent :: Reducible a => a -> a -> Bool
confluent e0 e1 = case (eval' e0, eval' e1) of
  (Right e1', Right e0') -> e0' @= e1'
  _ -> False

e0 *= e1 = confluent e0 e1

e0 *!= e1 = not (e0 *= e1)