{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Surface
  ( T.Tree (..),
    module Mean.Syntax.Surface,
  )
where

import Control.Monad.State
import Data.List (intercalate)
import Data.Set
import qualified Data.Tree.Binary.Preorder as T
import Debug.Trace (traceM)
import qualified Mean.Parser as P
import Mean.Syntax.Type
import Mean.Viz
import Text.Megaparsec.Debug (dbg)
import Text.PrettyPrint (char, parens, text, (<+>), (<>))
import Prelude hiding (Eq, GT, LT, (*), (<>))
import qualified Prelude as Prel

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

instance Pretty Binder where
  ppr p (Binder n t) = char 'λ' <> text (show n)

data UnOp = Neg deriving (Prel.Eq, Ord, Show)

data BinOp = Add | Sub | Mul | LT | GT | GTE | LTE | Eq | NEq | And | Or deriving (Prel.Eq, Ord, Show)

data Expr
  = ELit Lit
  | EVar Var
  | EBind Binder
  | ELam Binder Expr
  | EApp Expr Expr
  | ECond Expr Expr Expr
  | EUnOp UnOp Expr
  | EBinOp BinOp Expr Expr
  | ETree (T.Tree Expr)
  | ELitCase Expr [(Expr, Expr)]
  | ETyCase Expr [(Type, Expr)]
  | ESet (Set Expr)
  | ELet Var Expr Expr
  | EFix Var Expr
  deriving (Prel.Eq, Ord)

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
    ETree t -> text $ show e
    ELitCase e es -> text "litcase" <+> ppCase p e es
    ETyCase e es -> text "tycase" <+> ppCase p e es
    ESet s -> text $ show s
    ELet v e e' -> "let" <+> text (show v) <+> "=" <+> ppr p e <+> "in" <+> ppr p e'
    EFix v e -> text "fix" <+> text (show v) <+> text "in" <+> ppr p e

instance Show Expr where
  show = show . ppr 0

mkVar :: Name -> Var
mkVar v = Var v (v ++ "0")

mkEVar :: Name -> Expr
mkEVar = EVar . mkVar

(*) :: Expr -> Expr -> Expr
(*) = EApp

(~>) :: Expr -> Expr -> Expr
(EVar v) ~> e = ELam (Binder v TyNil) e
_ ~> _ = error "can't bind anything but a variable!"

infixl 9 *

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

pBinder :: P.Parser Binder
pBinder = do
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
  b <- pBinder
  P.symbol "."
  ELam b <$> pExpr

pELitCase :: ExprParser
pELitCase = P.try $ P.indentBlock P.spaceN p
  where
    pCase = do
      c <- pExpr
      P.reserved ":"
      r <- pExpr
      pure (c, r)
    p = do
      P.reserved "case"
      base <- pExpr
      P.reserved "of"
      pure $ P.IndentSome Nothing (pure . ELitCase base) pCase

pESet = do
  es <- P.curlies (pExpr `P.sepBy` (P.space >> P.char ',' >> P.space))
  pure $ ESet $ fromList es

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
    pEBind = EBind <$> pBinder
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
  [ P.parens pExpr,
    pELit,
    pEVar,
    pECond,
    pELam,
    pELitCase,
    pESet
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
      P.infixOpL "||" (EBinOp Or)
    ]
  ]

pExpr' :: ExprParser
pExpr' = P.makeExprParser pTerm operatorTable

pExpr :: ExprParser
pExpr = do
  exprs <- P.some pExpr'
  pure (foldl1 EApp exprs)