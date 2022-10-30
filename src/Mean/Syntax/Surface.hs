{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Surface
  ( T.Tree (..),
    T.fromList,
    module Mean.Syntax.Surface,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Monoid as M
import qualified Data.Set as Set
import qualified Data.Tree.Binary.Preorder as T
import Debug.Trace (trace, traceM)
import Mean.Context
import qualified Mean.Parser as P
import Mean.Syntax.Type
import Mean.Viz
import Mean.Walk
import Text.Megaparsec.Debug (dbg)
import Text.PrettyPrint (char, parens, text, (<+>), (<>))
import Prelude hiding (Eq, GT, LT, (*), (<>), (>))
import qualified Prelude as Prel

data Lit
  = LInt Int
  | LBool Bool
  deriving (Prel.Eq, Ord, Show)

data ABinder b t = Binder b t deriving (Prel.Eq, Ord)

type Binder b = ABinder b Type

data UnOp = Neg deriving (Prel.Eq, Ord, Show)

data BinOp = Add | Sub | Mul | LT | GT | GTE | LTE | Eq | NEq | And | Or | Impl deriving (Prel.Eq, Ord, Show)

data Domain e = Dom Type (Set.Set e)

data QRstr e = QRstr Var (Domain e)

data QExpr e
  = Univ [QRstr e] !e
  | Exis [QRstr e] !e

instance Prel.Eq e => Prel.Eq (Domain e) where
  (Dom t es) == (Dom t' es') = t == t' Prel.&& es == es'

instance Ord e => Ord (Domain e) where
  (Dom t es) <= (Dom t' es') = t <= t' Prel.&& es <= es'

instance Prel.Eq e => Prel.Eq (QRstr e) where
  (QRstr v d) == (QRstr v' d') = v == v' Prel.&& d == d'

instance Ord e => Ord (QRstr e) where
  (QRstr v d) <= (QRstr v' d') = v <= v' Prel.&& d <= d'

instance Prel.Eq e => Prel.Eq (QExpr e) where
  (Univ rs b) == (Univ rs' b') = rs == rs' Prel.&& b == b'
  (Exis rs b) == (Exis rs' b') = rs == rs' Prel.&& b == b'
  _ == _ = False

instance Ord e => Ord (QExpr e) where
  (Univ rs b) <= (Univ rs' b') = rs <= rs' Prel.&& b <= b'
  (Exis rs b) <= (Exis rs' b') = rs <= rs' Prel.&& b <= b'

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
  | ESet (Set.Set Expr)
  | EFix Var Expr
  | ETup [Expr]
  | EIdx Int -- TODO: parse me
  | EWild
  | EUndef
  | EDom (Domain Expr)
  | EQnt (QExpr Expr)
  | ENLam [Binder Var] Expr
  | ENApp Expr [Expr]
  deriving (Prel.Eq, Ord)

qnt :: ([QRstr Expr] -> Expr -> QExpr Expr) -> [(Var, Domain Expr)] -> Expr -> Expr
qnt q vs b = EQnt (q (fmap (uncurry QRstr) vs) b)

univ :: [(Var, Domain Expr)] -> Expr -> Expr
univ = qnt Univ

exis :: [(Var, Domain Expr)] -> Expr -> Expr
exis = qnt Exis

-- | The alternative to writing this by hand is to make
--   `Expr` a recursive datatype, take its fix point, and
--   derive a properly general functor. For the time being
--   this is simpler.
instance Walkable Expr where
  walkMC' f = f ctn
    where
      go = walkMC' f
      sgo s = Set.fromList <$> mapM go (Set.toList s)
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
        ETyCase e cs -> ETyCase <$> go e <*> mapM (\(b, e) -> do e' <- go e; pure (b, e')) cs
        EFix v e -> EFix v <$> go e
        EQnt q ->
          EQnt <$> case q of
            Exis r e -> Exis r <$> go e
            Univ r e -> Univ r <$> go e
        EDom (Dom t es) -> EDom <$> (Dom t <$> sgo es)
        -- non recursive inhabitants
        e' -> f pure e'

{-
foldWalk :: Monoid b => (Expr -> b) -> Expr -> b
foldWalk f = f'
  where
    go = foldWalk f
    cgo = M.mconcat . fmap go
    f' = \case
      EApp e0 e1 -> go e0 M.<> go e1
      ECond x y z -> go x M.<> go y M.<> go z
      EUnOp op e -> go e
      EBinOp op e0 e1 -> go e0 M.<> go e1
      ETup es -> cgo es
      ELam b e -> go e
      ETree t -> cgo (toList t)
      ESet es -> cgo (Set.toList es)
      ELitCase e cs -> go e M.<> M.mconcat (fmap (fmap go) cs)
      -- ETyCase e cs -> go e <*> mapM (\(b, e) -> do e' <- go e; pure (b, e')) cs
      EFix v e -> go e
      -- non recursive inhabitants
      e' -> f e'
-}

instance Pretty Lit where
  ppr p l = case l of
    LInt n -> text (show n)
    LBool b -> text (show b)

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
    where
      pq p r b = ppr p r <> brackets (ppr p b)

ppCase p c es =
  let pp (e0, e1) = ppr p e0 <+> text "->" <+> ppr p e1
   in ppr p c <> char ':' <+> text (intercalate " | " (show . pp <$> es))

instance Pretty a => Pretty (Set.Set a) where
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

instance Show b => Pretty (Binder b) where
  ppr p (Binder b t) = char 'λ' <> text (show b) <> angles (ppr p t)

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

infixr 8 +>

infixl 8 ~>

(&&) :: Expr -> Expr -> Expr
(&&) = EBinOp And

(||) :: Expr -> Expr -> Expr
(||) = EBinOp Or

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

-- λeb . e
churchLeaf =
  let [e, b] = mkEVar <$> ["e", "b"]
   in e ~> (b ~> e)

-- λxlreb . b(x)(l e b)(r e b)
typedChurchBranch ty =
  let [e, b@(EVar bV), x, l, r] = mkEVar <$> ["e", "b", "x", "l", "r"]
   in x ~> (l ~> (r ~> (e ~> ELam (Binder bV ty) (b * x * (l * e * b) * (r * e * b)))))

churchBranch = typedChurchBranch TyNil

mkTypedChurchTree :: Type -> T.Tree Expr -> Expr
mkTypedChurchTree ty t = case t of
  T.Leaf -> churchLeaf
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
  P.spaceN
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
  P.spaceN
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

after p fn = do
  mA <- P.observing p
  case mA of
    Left e -> fn >> P.parseError e
    Right a -> fn >> pure a

-- trees are prohibited as the expression nodes of trees,
-- as this would introduce syntactic ambiguity
pETree :: ExprParser
pETree = do
  s <- get
  put (s {P.inTree = True})
  t <- after (pTree pENode) (put $ s {P.inTree = False})
  pure $ ETree t
  where
    pEBind = EBind <$> pVarBinder
    pENode = P.try pExpr P.<|> pEBind

pTree pNode =
  P.choice
    [P.try pLeafNode, P.try pUnNode, pBiNode]
  where
    mkLeafNode e = T.Node e T.Leaf T.Leaf
    mkUnNode e l = T.Node e l T.Leaf

    pLeafNode = P.brackets $ mkLeafNode <$> pNode
    pUnNode = P.brackets $ do
      e <- pNode
      mkUnNode e <$> pTree pNode

    pBiNode = P.brackets $ do
      e <- pNode
      l <- pTree pNode
      T.Node e l <$> pTree pNode

terms =
  [ P.try $ P.parens pExpr,
    pELit,
    pEVar,
    pECond,
    pELam,
    pELitCase,
    pETyCase,
    pESet,
    pETup,
    P.reserved "undef" >> pure EUndef
  ]

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
