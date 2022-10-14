{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Surface
  ( T.Tree (..),
    T.fromList,
    module Mean.Syntax.Surface,
  )
where

import Control.Monad.State
import Data.Bifunctor (second)
import Data.List (intercalate)
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

data Binder b = Binder b Type deriving (Prel.Eq, Ord)

data UnOp = Neg deriving (Prel.Eq, Ord, Show)

data BinOp = Add | Sub | Mul | LT | GT | GTE | LTE | Eq | NEq | And | Or deriving (Prel.Eq, Ord, Show)

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
  | ELet Var Expr Expr
  | EFix Var Expr
  | ETup [Expr]
  | EIdx Int
  | EWildcard
  | EUndef
  deriving (Prel.Eq, Ord)

instance Walkable Expr where
  walkMC' f expr = ctn
    where
      go = walkMC' f
      ctn =
        f expr $ \case
          EApp e0 e1 -> EApp <$> go e0 <*> go e1
          ECond x y z -> ECond <$> go x <*> go y <*> go z
          EUnOp op e -> EUnOp op <$> go e
          EBinOp op e0 e1 -> EBinOp op <$> go e0 <*> go e1
          ETree t -> ETree <$> mapM go t
          ELitCase e cs -> ELitCase <$> go e <*> mapM (mapM go) cs
          ESet es -> ESet . Set.fromList <$> mapM go (Set.toList es)
          ETup es -> ETup <$> mapM go es
          ELam b e -> ELam b <$> go e
          ETyCase e cs -> ETyCase <$> go e <*> mapM (\(b, e) -> do e' <- go e; pure (b, e')) cs
          -- ELet Var Expr Expr
          EFix v e -> EFix v <$> go e
          e' -> pure e'

instance Pretty Lit where
  ppr p l = case l of
    LInt n -> text (show n)
    LBool b -> text (show b)

ppCase p c es =
  let pp (e0, e1) = ppr p e0 <+> text "->" <+> ppr p e1
   in ppr p c <> char ':' <+> text (intercalate " | " (show . pp <$> es))

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
      where
        infx p e0 e1 op = ppr p e0 <+> text op <+> ppr p e1
    ECond x y z -> text "if" <+> ppr p x <+> text "then" <+> ppr p y <+> text "else" <+> ppr p z
    ETree t -> text $ T.drawTree t
    ELitCase e es -> text "litcase" <+> ppCase p e es
    ETyCase e es -> text "tycase" <+> ppCase p e es
    ESet s -> text $ show s
    ELet v e e' -> "let" <+> text (show v) <+> "=" <+> ppr p e <+> "in" <+> ppr p e'
    EFix v e -> text "fix" <+> text (show v) <+> text "in" <+> ppr p e
    ETup es -> parens $ text (intercalate ", " (show <$> es))
    EIdx i -> brackets (text $ show i)
    EWildcard -> text "_"

instance Show Expr where
  show = show . ppr 0

instance Show b => Pretty (Binder b) where
  ppr p (Binder b t) = char 'λ' <> text (show b)

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
      tT = mkTv "T"
      tA = mkTv "A"
      tF = TyFun tT tT
      tX = TyFun tA (TyFun tA tT)
   in -- in (f, tF) +> (((x, tX) +> (f * (x * x))) * ((x, tX) +> (f * (x * x))))
      f ~> ((x ~> (f * (x * x))) * (x ~> (f * (x * x))))

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

pVar :: P.Parser Var
pVar = mkVar <$> P.identifier

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

pWildcardBinder = P.reserved "_" >> pure (Binder EWildcard TyWild)

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

pESet = do
  es <- P.curlies (P.commaSep pExpr)
  pure $ ESet $ Set.fromList es

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
    pETup
  ]

pTerm :: ExprParser
pTerm = do
  s <- get
  P.choice $
    [pETree | not (P.inTree s)] ++ terms

operatorTable :: [[P.Operator P.Parser Expr]]
operatorTable =
  [ [P.prefixOp "!" (EUnOp Neg)],
    [ P.infixOpL "==" (EBinOp Eq),
      P.infixOpL "!=" (EBinOp NEq),
      P.infixOpL "&&" (EBinOp And),
      P.infixOpL "||" (EBinOp Or),
      P.infixOpL "+" (EBinOp Add),
      P.infixOpL "-" (EBinOp Sub),
      P.infixOpL "*" (EBinOp Mul),
      P.infixOpL "<" (EBinOp LT),
      P.infixOpL "<=" (EBinOp LTE),
      P.infixOpL ">" (EBinOp GT),
      P.infixOpL ">=" (EBinOp GTE)
    ]
  ]

pExpr' :: ExprParser
pExpr' = P.makeExprParser pTerm operatorTable

pExpr :: ExprParser
pExpr = do
  exprs <- P.some pExpr'
  pure (foldl1 EApp exprs)
