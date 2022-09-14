{-# LANGUAGE GADTs, ConstraintKinds, FlexibleContexts, FlexibleInstances, OverloadedStrings #-}

module Mean.Core where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Set ((\\))
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Debug.Trace (traceM)
import qualified Mean.Parser as P
import Mean.Viz ( Pretty(ppr), angles, anglesIf )
import Text.PrettyPrint
    ( Doc, (<+>), (<>), brackets, char, parens, text )
import Prelude hiding (Eq, GT, LT, (<>), (>), (*))
import qualified Prelude as Prel

type Evaluation = ExceptT EvalError Identity

class Reducible a where
  reduce :: a -> Evaluation CoreExpr

type Expressible a = (Prel.Eq a, Reducible a, Pretty a)

data EvalError
  = UnboundVariable Name
  | NotTruthy CoreExpr
  deriving (Prel.Eq)

instance Show EvalError where
  show (UnboundVariable n) = "Unbound variable: " ++ show n
  show (NotTruthy e) = "Not truthy: " ++ show e

class FV a where
  fv :: a -> Set.Set Name

newtype TyVar = TV String
  deriving (Show, Prel.Eq, Ord)

data Type
  = TyVar TyVar
  | TyCon String
  | TyFun Type Type
  | TyNil
  deriving (Prel.Eq, Ord)

data TyScheme = Forall [TyVar] Type
  deriving (Prel.Eq, Ord)

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
  deriving (Prel.Eq, Ord, Show)

data Var = Var Name Name

instance Prel.Eq Var where
  (Var _ v) == (Var _ v') = v == v'

instance Ord Var where
  compare (Var _ v0) (Var _ v1) = compare v0 v1

instance Show Var where
  show (Var vPub vPri) = vPub

data Binder = Binder Var Type deriving (Prel.Eq, Ord)

data Lambda a where Lam :: Expressible a => Binder -> a -> Lambda a

data App a where App :: Expressible a => a -> a -> App a

data Cond a where Cond :: Expressible a => a -> a -> a -> Cond a

data BinOp = Add | Sub | Mul | LT | GT | LTE | GTE | Eq deriving (Show, Prel.Eq)

binOpTy op = case op of Add -> tyInt; Sub -> tyInt; Mul -> tyInt; _ -> tyBool

instance Prel.Eq (Lambda a) where
  (Lam b0 e0) == (Lam b1 e1) = b0 == b1 && e0 == e1

instance Prel.Eq (App a) where
  (App e0 e1) == (App e0' e1') = e0 == e0' && e1 == e1'

instance Prel.Eq (Cond a) where
  (Cond a0 a1 a2) == (Cond a0' a1' a2') = [a0, a1, a2] == [a0', a1', a2']

data CoreExpr
  = CLit Lit
  | CVar Var
  | CBind Binder
  | CLam (Lambda CoreExpr)
  | CApp (App CoreExpr)
  | CCond (Cond CoreExpr)
  | CBinOp BinOp CoreExpr CoreExpr
  deriving (Prel.Eq)

instance Pretty TyVar where
  ppr _ (TV t) = text t

instance Pretty [TyVar] where
  ppr p (t : ts) = ppr p t <> char ',' <> ppr p ts
  ppr p [] = ""

instance Pretty Type where
  ppr p (TyCon t) = anglesIf (p == 0) $ text t
  ppr p (TyVar t) = anglesIf (p == 0) $ ppr p t
  ppr p (TyFun a b) = angles $ ppr (p + 1) a <> char ',' <> ppr (p + 1) b
  ppr p TyNil = text "TyNil"

instance Pretty Binder where
  ppr p (Binder n t) = char 'λ' <> text (show n)

instance Pretty (Lambda CoreExpr) where
  ppr p (Lam b e) =
    ppr p b <> case e of
      CLam Lam {} -> ppr (p + 1) e
      _ -> brackets (ppr (p + 1) e)

instance (Pretty a) => Pretty (App a) where
  ppr p (App e0 e1) = ppr p e0 <> parens (ppr p e1)

instance Pretty Lit where
  ppr p l = case l of
    LInt n -> text (show n)
    LBool b -> text (show b)

instance Pretty (Cond a) where
  ppr p (Cond x y z) = text "if" <+> ppr p x <+> text "then" <+> ppr p y <+> text "else" <+> ppr p z

instance Pretty CoreExpr where
  ppr p e = case e of
    CLit l -> ppr p l
    CVar s -> text (show s)
    CBind b -> ppr p b
    CLam l -> ppr p l
    CApp a -> ppr p a
    CCond c -> ppr p c
    CBinOp op e0 e1 -> let
      op' = case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        LT -> "<"
        LTE -> "<="
        GT -> ">"
        GTE -> ">="
        Eq -> "=="
      in  ppr p e0 <+> text op' <+> ppr p e1

instance Pretty TyScheme where
  ppr p (Forall tvs ty) = "Forall" <+> brackets (ppr p tvs) <> ":" <+> ppr p ty

instance Show CoreExpr where
  show = show . ppr 0

instance Show Type where
  show = show . ppr 0

instance Show TyScheme where
  show = show . ppr 0

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

zero = CLit lZero
one = CLit lOne

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

e :: CoreExpr
e = mkCVar "e"

b :: CoreExpr
b = mkCVar "b"

eq :: CoreExpr -> CoreExpr -> CoreExpr
eq = CBinOp Eq

(===) :: CoreExpr -> CoreExpr -> CoreExpr
(===) = eq

(?) x y z = CCond (Cond x y z)

e > e' = e e'

instance FV CoreExpr where
  fv e = case e of
    CVar (Var _ v) -> Set.singleton v
    CLam (Lam (Binder (Var _ v) _) body) -> fv body \\ Set.singleton v
    CApp (App e0 e1) -> fv [e0, e1]
    CCond (Cond x y z) -> fv [x, y, z]
    CBinOp _ e0 e1 -> fv [e0, e1]
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

bool :: CoreExpr -> Bool
bool e = case e of
  (CLit (LBool b)) -> b
  _ -> error "can only extract bool from literal bool"

-- e'[e/v]
substitute e cv@(CVar v) e' =
  let sub' = substitute e cv
  in case e' of
    -- relevant base case
    CVar v' | v' == v -> e
    -- induction
    CApp (App e0 e1) -> mkCApp (sub' e0) (sub' e1)
    CCond (Cond x y z) -> sub' x ? sub' y > sub' z
    CBinOp op e0 e1 -> CBinOp op (sub' e0) (sub' e1)
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
        -- traceM ("\n0 " ++ show expr)
        e <- reduce (substitute e1 (CVar v) body)
        -- traceM ("\n1 " ++ show e)
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
    -- (3) some operations are defined for literals
    CBinOp op e0 e1 -> do
      e0' <- reduce e0
      e1' <- reduce e1
      pure $ case (e0', e1') of
        (CLit (LBool b0), CLit (LBool b1)) -> CLit $ case op of
          Eq -> LBool $ (==) b0 b1
        (CLit (LInt i0), CLit (LInt i1)) -> CLit $ case op of
          Add -> LInt $ (+) i0 i1
          Sub -> LInt $ (-) i0 i1
          Mul -> LInt $ (Prel.*) i0 i1
          LT -> LBool $ (<) i0 i1
          LTE -> LBool $ (<=) i0 i1
          GT -> LBool $ (Prel.>) i0 i1
          GTE -> LBool $ (>=) i0 i1
          Eq -> LBool $ (==) i0 i1
        _ -> CBinOp op e0' e1'
    CCond (Cond x y z) -> do
      x' <- reduce x
      case x' of
        CLit LBool {} -> reduce $ if bool x' then y else z
        _ -> pure $ CCond (Cond x' y z)
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
  (CBinOp op e0a e1a, CBinOp op' e0b e1b) -> (op == op') && (e0a @= e0b) && (e1a @= e1b)
  (CCond (Cond x0 y0 z0), CCond (Cond x1 y1 z1)) -> (x0 @= x1) && (y0 @= y1) && (z0 @= z1)
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

-------------------------------------------------------------------------------
-- Parsing (Types)
-------------------------------------------------------------------------------

type TyParser = P.Parser Type

pTyTerm :: TyParser
pTyTerm =
  P.choice
    [ P.angles pType,
      P.reserved "t" >> pure tyBool,
      P.reserved "n" >> pure tyInt,
      P.titularIdentifier <&> TyVar . TV,
      P.identifier <&> TyCon
    ]

tyNil :: TyParser
tyNil = pure TyNil

pType :: TyParser
pType = P.makeExprParser pTyTerm tyOps
  where
    tyOps =
      [ [P.infixOpR "," TyFun]
      ]

pTypeAssignment :: TyParser
pTypeAssignment = (P.reserved ":" >> pType) P.<|> tyNil

pOptionalTypeAssignment :: TyParser
pOptionalTypeAssignment = pTypeAssignment P.<|> tyNil

-------------------------------------------------------------------------------
-- Parsing (Terms)
-------------------------------------------------------------------------------

type CExprParser = P.Parser CoreExpr

type LitParser = P.Parser Lit

pBool :: LitParser
pBool =
  (P.reserved "True" >> pure lTrue)
    P.<|> (P.reserved "False" >> pure lFalse)

pInt :: LitParser
pInt = LInt . fromIntegral <$> P.integer

pLit :: LitParser
pLit = P.choice [pInt, pBool]

pCLit :: CExprParser
pCLit = CLit <$> pLit

pVar :: P.Parser Var
pVar = mkVar <$> P.identifier

pCVar :: CExprParser
pCVar = CVar <$> pVar

pBinder :: P.Parser Binder
pBinder = do
  P.symbol "\\"
  n <- P.identifier
  Binder (mkVar n) <$> pOptionalTypeAssignment

pLam :: Expressible a => P.Parser (a -> Lambda a)
pLam = do
  b <- pBinder
  P.symbol "."
  pure $ Lam b

pCLam :: CExprParser
pCLam = do
  lam <- pLam
  CLam . lam <$> pCExpr

pCond pExpr = do
  P.reserved "if"
  x <- pExpr
  P.reserved "then"
  y <- pExpr
  P.reserved "else"
  Cond x y <$> pExpr

pCCond = CCond <$> pCond pCExpr

pCTerm :: CExprParser
pCTerm =
  P.choice
    [ P.parens pCExpr,
      pCLit,
      pCVar,
      pCLam,
      pCCond
    ]

cOperatorTable :: [[P.Operator P.Parser CoreExpr]]
cOperatorTable =
  [[ P.infixOpL "==" (CBinOp Eq) ]]

pCExpr' :: CExprParser
pCExpr' = P.makeExprParser pCTerm cOperatorTable

pCExpr :: CExprParser
pCExpr = do
  exprs <- P.some pCExpr'
  pure (foldl1 (\e0 e1 -> CApp $ App e0 e1) exprs)
