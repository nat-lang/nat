{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Syntax.Surface
  ( T.Tree (..),
    T.fromList,
    module Nat.Syntax.Surface,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Monoid as M
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Tree.Binary.Preorder as T
import Debug.Trace (trace, traceM)
import GHC.Float (fromRat)
import Nat.Context
import qualified Nat.Parser as P
import Nat.Syntax.Type
import Nat.Viz
import Nat.Walk
import Numeric (showFFloat)
import Text.Megaparsec.Debug (dbg)
import Text.PrettyPrint (char, parens, text, (<+>), (<>))
import Prelude hiding (Eq, GT, LT, (*), (<>), (>))
import qualified Prelude as Prel

data Lit
  = LInt Rational
  | LBool Bool
  deriving (Prel.Eq, Ord, Show)

data ABinder b t = Binder b t deriving (Prel.Eq, Ord)

type Binder b = ABinder b Type

data UnOp = Neg | Len deriving (Prel.Eq, Ord, Show)

data BinOp
  = -- universal
    Eq
  | NEq
  | -- ints
    Add
  | Sub
  | Mul
  | Mod
  | LT
  | GT
  | GTE
  | LTE
  | -- ints & sets
    Div
  | -- bools & sets
    And
  | Or
  | -- bools
    Impl
  | -- sets
    Mem
  | SubS
  | SupS
  | PSubS
  | PSupS
  deriving (Prel.Eq, Ord, Show)

data Domain e = Dom Type (Set e)

data QRstr e = QRstr Var e

data QExpr e
  = Univ [QRstr e] !e
  | Exis [QRstr e] !e
  | Iota [QRstr e] !e

instance Prel.Eq e => Prel.Eq (Domain e) where
  (Dom t es) == (Dom t' es') = t == t' Prel.&& es == es'

instance Ord e => Ord (Domain e) where
  (Dom t es) <= (Dom t' es') = t <= t' Prel.&& es <= es'

instance Prel.Eq e => Prel.Eq (QRstr e) where
  (QRstr v d) == (QRstr v' d') = v == v' Prel.&& d == d'

instance Ord e => Ord (QRstr e) where
  (QRstr v d) <= (QRstr v' d') = v <= v' Prel.&& d <= d'

qEq rs rs' b b' = rs == rs' Prel.&& b == b'

instance Prel.Eq e => Prel.Eq (QExpr e) where
  (Univ rs b) == (Univ rs' b') = qEq rs rs' b b'
  (Exis rs b) == (Exis rs' b') = qEq rs rs' b b'
  (Iota rs b) == (Iota rs' b') = qEq rs rs' b b'
  _ == _ = False

qOrd rs rs' b b' = rs <= rs' Prel.&& b <= b'

instance Ord e => Ord (QExpr e) where
  (Univ rs b) <= (Univ rs' b') = qOrd rs rs' b b'
  (Exis rs b) <= (Exis rs' b') = qOrd rs rs' b b'
  (Iota rs b) <= (Iota rs' b') = qOrd rs rs' b b'

data Expr
  = ELit Lit
  | EVar Var
  | EBind (Binder Var)
  | ELam (Binder Var) Expr
  | EApp Expr Expr
  | ECond Expr Expr Expr
  | EUnOp UnOp Expr
  | EBinOp BinOp Expr Expr
  | ETree (T.Tree Expr)
  | ELitCase Expr [(Expr, Expr)]
  | ETyCase Expr [(Binder Expr, Expr)]
  | ESet (Set Expr)
  | EFix Var Expr
  | ETup [Expr]
  | EIdx Int -- TODO: parse me
  | EWild
  | EUndef
  | EDom (Domain Expr)
  | EQnt (QExpr Expr)
  | EFun [Binder Var] Expr
  | EInv Expr [Expr]
  deriving (Prel.Eq, Ord)

qnt :: ([QRstr Expr] -> Expr -> QExpr Expr) -> [(Var, Expr)] -> Expr -> Expr
qnt q vs b = EQnt (q (fmap (uncurry QRstr) vs) b)

univ :: [(Var, Expr)] -> Expr -> Expr
univ = qnt Univ

exis :: [(Var, Expr)] -> Expr -> Expr
exis = qnt Exis

the :: [(Var, Expr)] -> Expr -> Expr
the = qnt Iota

-- | The alternative to writing this by hand is to make
--   `Expr` a recursive datatype, take its fix point,
--   derive a properly general functor, and then generalize
--   that to a traversal
instance Walkable Expr where
  walkMC' f = f ctn
    where
      ctn = \case
        EApp e0 e1 -> EApp <$> go e0 <*> go e1
        ECond x y z -> ECond <$> go x <*> go y <*> go z
        EUnOp op e -> EUnOp op <$> go e
        EBinOp op e0 e1 -> EBinOp op <$> go e0 <*> go e1
        ESet es -> ESet <$> sgo es
        ETup es -> ETup <$> mapM go es
        ELam b e -> ELam b <$> go e
        ETree t -> ETree <$> mapM go t
        ELitCase e cs -> ELitCase <$> go e <*> mapM (mapM go) cs
        ETyCase e cs -> ETyCase <$> go e <*> mapM cgo cs
        EFix v e -> EFix v <$> go e
        EQnt q ->
          EQnt <$> case q of
            Exis rs e -> qgo Exis rs e
            Univ rs e -> qgo Univ rs e
        EDom (Dom t es) -> EDom <$> (Dom t <$> sgo es)
        -- non recursive inhabitants
        e' -> f pure e'
      sgo s = Set.fromList <$> mapM go (Set.toList s)
      rgo (QRstr v e) = QRstr v <$> go e
      qgo q rs e = q <$> mapM rgo rs <*> go e
      cgo (b, e) = do e' <- go e; pure (b, e')
      go = walkMC' f

instance Pretty Lit where
  ppr p l = case l of
    LInt n -> text (truncate n)
    LBool b -> text (show b)
    where
      isInt x = x == fromInteger (round x)
      truncate n = showFFloat (Just $ if isInt n then 0 else 2) (fromRat n) ""

isTyNil TyNil = True
isTyNil _ = False

instance Show b => Pretty (Binder b) where
  ppr p (Binder b t) = char 'λ' <> text (show b) <> if isTyNil t then "" else angles (ppr p t)

instance Pretty a => Pretty (Domain a) where
  ppr p (Dom _ es) = ppr p es

instance Pretty a => Pretty (QRstr a) where
  ppr n (QRstr var d) = text (show var) <+> text "∈" <+> ppr n d

instance Pretty a => Pretty [QRstr a] where
  ppr n rs = text $ intercalate ", " (show . ppr n <$> rs)

instance Pretty a => Pretty (QExpr a) where
  ppr p qe = case qe of
    Univ r b -> "∀" <> pq p r b
    Exis r b -> "∃" <> pq p r b
    Iota r b -> "ι" <> pq p r b
    where
      pq p r b = ppr p r <> brackets (ppr p b)

ppCase p c es =
  let pp (e0, e1) = ppr p e0 <+> text "->" <+> ppr p e1
   in ppr p c <> char ':' <+> text (intercalate " | " (show . pp <$> es))

instance Pretty a => Pretty (Set a) where
  ppr p s = curlies $ text (intercalate ", " (show . ppr 0 <$> Set.toList s))

instance Pretty Expr where
  ppr p e = case e of
    ELit l -> ppr p l
    EVar v -> text $ show v
    EBind b -> ppr p b
    ELam b e ->
      ppr p b <> case e of
        ELam {} -> ppr (p + 1) e
        _ -> brackets (ppr (p + 1) e)
    EApp e0 e1 -> ppr p e0 <> parens (ppr p e1)
    EUnOp op e -> char ppOp <> ppr p e
      where
        ppOp = case op of
          Neg -> '¬'
    EBinOp op e0 e1 -> infx p e0 e1 $ case op of
      Add -> "+"
      Sub -> "-"
      Mul -> "*"
      Div -> "/"
      Mod -> "%"
      LT -> "<"
      LTE -> "<="
      GT -> ">"
      GTE -> ">="
      Eq -> "=="
      NEq -> "!="
      And -> "&"
      Or -> "|"
      Impl -> "=>"
      where
        infx p e0 e1 op = ppr p e0 <+> text op <+> ppr p e1
    ECond x y z -> text "if" <+> ppr p x <+> text "then" <+> ppr p y <+> text "else" <+> ppr p z
    ETree t -> text $ T.drawTree t
    ELitCase e es -> text "litcase" <+> ppCase p e es
    ETyCase e es -> text "tycase" <+> ppCase p e es
    ESet s -> ppr p s
    EFix v e -> text "fix" <+> text (show v) <+> text "in" <+> ppr p e
    ETup es -> parens $ text (intercalate ", " (show <$> es))
    EIdx i -> brackets (text $ show i)
    EWild -> text "_"
    EUndef -> text "undef"
    EDom d -> ppr p d
    EQnt q -> ppr p q

instance Show Expr where
  show = show . ppr 0

instance Show b => Show (Binder b) where
  show = show . ppr 0

mkEVar :: Name -> Expr
mkEVar = EVar . mkVar

(*) :: Expr -> Expr -> Expr
(*) = EApp

(&>) :: (Expr, Type) -> Binder Var
(&>) (EVar v, t) = Binder v t
(&>) _ = error "can't bind anything but a variable!"

(+>) :: (Expr, Type) -> Expr -> Expr
(+>) (e, t) = ELam $ (&>) (e, t)

(~>) :: Expr -> Expr -> Expr
(~>) e = (+>) (e, TyNil)

infixl 9 *

infixl 8 &>

infixr 8 +>, ~>

(&&&) :: Expr -> Expr -> Expr
(&&&) = EBinOp And

(|||) :: Expr -> Expr -> Expr
(|||) = EBinOp Or

(==>) = EBinOp Impl

(===) = EBinOp Eq

(!==) :: Expr -> Expr -> Expr
(!==) = EBinOp NEq

not' :: Expr -> Expr
not' = EUnOp Neg

(?) :: Expr -> Expr -> (Expr -> Expr)
x ? y = ECond x y

(>) :: (Expr -> Expr) -> Expr -> Expr
e > e' = e e'

churchLeaf' e b = e ~> (b ~> e)

-- λeb . e
churchLeaf =
  let [e, b] = mkEVar <$> ["e", "b"]
   in churchLeaf' e b

typedChurchBranch' x l r e b@(EVar bV) ty = x ~> (l ~> (r ~> (e ~> ELam (Binder bV ty) (b * x * (l * e * b) * (r * e * b)))))

-- | The church encoding of a tree yields a catamorphism when
-- reduced to normal form.
--    Leaf: λeb. e
--    Branch: λxlreb. b(x)(l e b)(r e b)
--
-- ... λeb. b(x)(l e b)(r e b)
typedChurchBranch =
  let [e, b, x, l, r] = mkEVar <$> ["e", "b", "x", "l", "r"]
   in typedChurchBranch' x l r e b

churchBranch = typedChurchBranch TyNil

mkTypedChurchTree :: Type -> T.Tree Expr -> Expr
mkTypedChurchTree ty t = case t of
  T.Leaf -> churchLeaf
  -- expr trees stored as data are treated as branches
  T.Node (ETree t') T.Leaf T.Leaf -> mkTypedChurchTree ty t'
  T.Node e l r -> typedChurchBranch ty * e * mkTypedChurchTree ty l * mkTypedChurchTree ty r

mkChurchTree :: T.Tree Expr -> Expr
mkChurchTree = mkTypedChurchTree TyNil

-- λf. (λx. f (x x)) (λx . f (x x))
yCombinator =
  let f = mkEVar "f"
      x = mkEVar "x"
   in f ~> ((x ~> (f * (x * x))) * (x ~> (f * (x * x))))

mkFixPoint v e = yCombinator * (EVar v ~> e)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

type LitParser = P.Parser Lit

pBool :: LitParser
pBool =
  (P.reserved "True" >> pure (LBool True))
    P.<|> (P.reserved "False" >> pure (LBool False))

pInt :: LitParser
pInt = LInt . fromIntegral <$> P.integer

pLit :: LitParser
pLit = P.choice [pInt, pBool]

pVarBinder :: P.Parser (Binder Var)
pVarBinder = do
  P.symbol "\\"
  n <- P.identifier
  Binder (mkVar n) <$> pOptionalType

type ExprParser = P.Parser Expr

pEVar = EVar <$> pVar

pELit = ELit <$> pLit

pECond :: ExprParser
pECond = do
  P.reserved "if"
  x <- pExpr
  P.reserved "then"
  y <- pExpr
  P.reserved "else"
  ECond x y <$> pExpr

pELam :: ExprParser
pELam = do
  b <- pVarBinder
  P.symbol "."
  ELam b <$> pExpr

pETup :: ExprParser
pETup = do
  es <- P.parens (P.commaSep pExpr)
  pure $ ETup es

pELitCase :: ExprParser
pELitCase = do
  P.reserved "case"
  base <- pExpr
  P.reserved "of"
  P.someSpace
  cases <- P.pipeSep pCase
  pure $ ELitCase base cases
  where
    pCase = do
      c <- pTerm
      P.reserved "->"
      r <- pExpr
      pure (c, r)

pWildcardBinder = P.reserved "_" >> pure (Binder EWild TyWild)

pExprBinder :: P.Parser (Binder Expr)
pExprBinder = do
  e <- pTerm
  P.reserved ":"
  Binder e <$> pType

pETyCase :: ExprParser
pETyCase = do
  P.reserved "tycase"
  base <- pExpr
  P.reserved "of"
  P.someSpace
  cases <- P.pipeSep pCase
  pure $ ETyCase base cases
  where
    pCase = do
      b <- pWildcardBinder P.<|> pExprBinder
      P.reserved "->"
      r <- pExpr
      pure (b, r)

pSet = do
  es <- P.curlies (P.commaSep pExpr)
  pure $ Set.fromList es

pESet = ESet <$> pSet

pQRstr = do
  v <- pVar
  P.reserved "in"
  QRstr v <$> pExpr

pQnt keyw q = do
  P.reserved keyw
  rstrs <- P.commaSep pQRstr
  P.symbol "."
  q rstrs <$> pExpr

pUniv = pQnt "forall" Univ

pExis = pQnt "exists" Exis

pIota = pQnt "the" Iota

pEQnt = EQnt <$> P.choice [pUniv, pExis, pIota]

after p fn = do
  mA <- P.observing p
  case mA of
    Left e -> fn >> P.parseError e
    Right a -> fn >> pure a

pETree :: ExprParser
pETree = ETree <$> pTree pENode
  where
    tExpr = P.try (P.choice unitaryTerms) P.<|> P.parens pExpr
    pEBind = EBind <$> pVarBinder
    pENode = tExpr P.<|> pEBind

pTree pData =
  P.choice
    [P.try pLeafNode, P.try pUnaryNode, pBinaryNode]
  where
    mkLeafNode e = T.Node e T.Leaf T.Leaf
    mkUnaryNode e l = T.Node e l T.Leaf

    pLeafNode =
      P.brackets $
        mkLeafNode <$> pData

    pUnaryNode =
      P.brackets $
        mkUnaryNode <$> pData <*> pTree pData

    pBinaryNode =
      P.brackets $
        T.Node <$> pData <*> pTree pData <*> pTree pData

unitaryTerms =
  [ pELit,
    pEVar,
    P.reserved "undef" >> pure EUndef,
    pESet
  ]

terms =
  [ P.try $ P.parens pExpr,
    pECond,
    pELam,
    pELitCase,
    pETyCase,
    pETup
  ]
    ++ unitaryTerms

pTerm :: ExprParser
pTerm = do
  s <- get
  P.choice $ [pETree | not (P.inTree s)] ++ terms

operatorTable :: [[P.Operator P.Parser Expr]]
operatorTable =
  [ [P.prefixOp "!" (EUnOp Neg)],
    [ P.infixOpL "==" (EBinOp Eq),
      P.infixOpL "!=" (EBinOp NEq),
      P.infixOpL "&&" (EBinOp And),
      P.infixOpL "||" (EBinOp Or),
      P.infixOpL "=>" (EBinOp Impl),
      P.infixOpL "+" (EBinOp Add),
      P.infixOpL "-" (EBinOp Sub),
      P.infixOpL "*" (EBinOp Mul),
      P.infixOpL "/" (EBinOp Div),
      P.infixOpL "%" (EBinOp Mod),
      P.infixOpL "<=" (EBinOp LTE),
      P.infixOpL ">=" (EBinOp GTE),
      P.infixOpL "<" (EBinOp LT),
      P.infixOpL ">" (EBinOp GT)
    ]
  ]

pExpr' :: ExprParser
pExpr' = P.makeExprParser pTerm operatorTable

pExpr :: ExprParser
pExpr = do
  exprs <- P.some pExpr'
  pure (foldl1 EApp exprs)
