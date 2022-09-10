{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances #-}

module Mean.Core where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Prelude hiding ((&&), (||), Eq)
import qualified Prelude as Prel

type Evaluation = ExceptT EvalError Identity

class Reducible a where
  reduce :: a -> Evaluation CoreExpr
  -- e''[e/']
  -- substitute :: a -> a -> a -> a

data EvalError
  = UnboundVariable Name
  | NotTruthy CoreExpr

instance Show EvalError where
  show (UnboundVariable n) = "Unbound variable: " ++ show n

class FV a where
  fv :: a -> Set.Set Name

newtype TyVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TyVar TyVar
  | TyCon String
  | TyFun Type Type
  | TyNil
  deriving (Eq, Ord)

data TyScheme = Forall [TyVar] Type
  deriving (Eq, Ord)

mkUnqScheme :: Type -> TyScheme
mkUnqScheme = Forall []

mkTv :: String -> Type
mkTv = TyVar . TV

tyInt, tyBool :: Type
tyInt = TyCon "n"
tyBool = TyCon "t"

type Name = String

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Ord, Show)

data Var = Var Name Name

instance Eq Var where
  (Var _ v) == (Var _ v') = v == v'

instance Ord Var where
  compare (Var _ v0) (Var _ v1) = compare v0 v1

instance Show Var where
  show (Var vPub vPri) = vPub

data Binder = Binder Var Type deriving (Eq, Ord)

data Lambda a where Lam :: Reducible a => Binder -> a -> Lambda a

data App a where App :: Reducible a => a -> a -> App a

data Eq a where Eq :: Reducible a => a -> a -> Eq a

data CoreExpr
  = CLit Lit
  | CVar Var
  | CBind Binder
  | CLam (Lambda CoreExpr)
  | CApp (App CoreExpr)
  | CEq (Eq CoreExpr)

instance Prel.Eq CoreExpr where
  e == e' = case (e, e') of
    (CLit {}, CLit {}) -> e == e'
    _ -> False

mkCBool :: Bool -> CoreExpr
mkCBool = CLit . LBool

mkVar :: Name -> Var
mkVar v = Var v (v ++ "0")

mkCVar :: Name -> CoreExpr
mkCVar v = CVar $ mkVar v

mkCLam :: Binder -> CoreExpr -> CoreExpr
mkCLam b = CLam . Lam b

mkCApp :: CoreExpr -> CoreExpr -> CoreExpr
mkCApp e = CApp . App e

mkBinder :: CoreExpr -> Binder
mkBinder (CVar v) = Binder v TyNil
mkBinder _ = error "can't bind anything but a variable!"

mkCBind :: CoreExpr -> CoreExpr
mkCBind = CBind . mkBinder

mkFn :: CoreExpr -> CoreExpr -> CoreExpr
mkFn = mkCLam . mkBinder

(*) :: CoreExpr -> CoreExpr -> CoreExpr
(*) = mkCApp

(~>) :: CoreExpr -> CoreExpr -> CoreExpr
(~>) = mkFn

infixl 9 *

infixl 8 ~>

lOne :: Lit
lOne = LInt 1

lZero :: Lit
lZero = LInt 0

lTrue :: Lit
lTrue = LBool True

lFalse :: Lit
lFalse = LBool False

true :: CoreExpr
true = CLit lTrue

false :: CoreExpr
false = CLit lFalse

f :: CoreExpr
f = mkCVar "f"

x :: CoreExpr
x = mkCVar "x"

y :: CoreExpr
y = mkCVar "y"

z :: CoreExpr
z = mkCVar "z"

m :: CoreExpr
m = mkCVar "m"

n :: CoreExpr
n = mkCVar "n"

l :: CoreExpr
l = mkCVar "l"

r :: CoreExpr
r = mkCVar "r"

p :: CoreExpr
p = mkCVar "p"

q :: CoreExpr
q = mkCVar "q"

-- λx.x
id :: CoreExpr
id = x ~> x

eq :: CoreExpr -> CoreExpr -> CoreExpr
eq = CEq

(===) :: CoreExpr -> CoreExpr -> CoreExpr
(===) = eq

instance FV CoreExpr where
  fv e = case e of
    CVar (Var _ v) -> Set.singleton v
    CLam (Lam (Binder (Var _ v) _) body) -> fv body \\ Set.singleton v
    CApp (App e0 e1) -> fv [e0, e1]
    CEq (Eq e0 e1) -> fv [e0, e1]
    _ -> Set.empty

instance FV [CoreExpr] where
  fv es = foldl1 Set.union (fv <$> es)

incrVarId :: Var -> Var
incrVarId (Var vPub vPri) = Var vPub $ init vPri ++ show (digitToInt (last vPri) + 1)

fresh :: Var -> Var -> Set.Set Name -> Var
fresh v0 v1 fv =
  let v0'@(Var _ v0'Pri) = incrVarId v0
   in if v0' == v1 Prel.|| Set.member v0'Pri fv
        then fresh v0' v1 fv
        else v0'

substitute e cv@(CVar v) e' =
  let sub' = substitute e cv
  in case e' of
        -- relevant base case
        CVar v' | v' == v -> e
        -- induction
        CApp (App e0 e1) -> mkCApp (sub' e0) (sub' e1)
        CEq (Eq e0 e1) -> CEq (sub' e0) (sub' e1)
        -- induction, but rename binder if it conflicts with fv(e)
        CLam (Lam b@(Binder v'@(Var _ v'Pri) t) body)
          | v /= v' ->
            let fvE = fv e
                fvB = fv body
            in if v'Pri `Set.member` fvE
                  then
                    let v'' = fresh v' v (fvE `Set.union` fvB)
                        body' = sub' $ substitute (CVar v'') (CVar v') body
                    in mkCLam (Binder v'' t) body'
                  else mkCLam b (sub' body)
        -- irrelevent base cases
        _ -> e'
substitute _ _ _ = error "can't substitute for anything but a variable!"

instance Reducible CoreExpr where
  --  Normal order reduction.
  reduce expr = case expr of
    -- (1) leftmost, outermost
    CApp (App e0 e1) -> case e0 of
      -- (1a) function application, beta reduce
      CLam (Lam (Binder v _) body) -> do
        -- traceM (show expr)
        e <- reduce (substitute e1 (CVar v) body)
        -- traceM (show e)
        pure e
      -- (1b) binders have a semantics of their own: they may be applied
      -- to terms, in which case they simply abstract a free variable.
      CBind b -> reduce (mkCLam b e1)
      -- (1c) normal form for lhs (free var), goto rhs
      CVar {} -> do
        e1' <- reduce e1
        pure (mkCApp e0 e1')
      -- (1d) lhs can be reduced
      _ -> do
        e0' <- reduce e0
        case e0' of
          -- (1d.1) normal form for lhs (app), goto rhs
          CApp {} -> do
            e1' <- reduce e1
            pure (mkCApp e0' e1')
          -- (1d.2) otherwise goto top
          _ -> reduce (mkCApp e0' e1)
    -- (2) simplify lambda body
    CLam (Lam b body) -> do
      body' <- reduce body
      pure $ mkCLam b body'
    -- (3) equality is defined for primitives
    CEq (Eq e0 e1) -> do
      e0' <- reduce e0
      e1' <- reduce e1
      case (e0', e1') of
        (CLit {}, CLit {}) -> pure $ mkCBool $ e0' == e1'
        _ -> pure $ CEq e0' e1'
    -- (4) var/literal
    _ -> pure expr

-- assumes e0 and e1 are in normal form
alphaEq :: CoreExpr -> CoreExpr -> Bool
alphaEq e0 e1 = case (e0, e1) of
  (CLam (Lam (Binder v0 _) body0), CLam (Lam (Binder v1 _) body1)) ->
    let v0' = CVar v0
        v1' = CVar v1
     in substitute v1' v0' body0 @= body1 Prel.|| substitute v0' v1' body1 @= body0
  (CApp (App e0a e0b), CApp (App e1a e1b)) -> e0a @= e1a Prel.&& e0b @= e1b
  (CVar v0, CVar v1) -> v0 == v1
  (CLit l0, CLit l1) -> l0 == l1
  _ -> False

(@=) :: CoreExpr -> CoreExpr -> Bool
e0 @= e1 = alphaEq e0 e1

(@!=) :: CoreExpr -> CoreExpr -> Bool
e0 @!= e1 = not (e0 @= e1)

eval :: Reducible a => a -> Either EvalError CoreExpr
eval e = runIdentity $ runExceptT $ reduce e

confluent :: Reducible a => a -> a -> Bool
confluent e0 e1 = case (eval e0, eval e1) of
  (Right e1', Right e0') -> e0' @= e1'
  _ -> False

(*=) :: Reducible a => a -> a -> Bool
e0 *= e1 = confluent e0 e1

(*!=) :: Reducible a => a -> a -> Bool
e0 *!= e1 = not (e0 *= e1)