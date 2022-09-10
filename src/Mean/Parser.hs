{-# LANGUAGE OverloadedStrings #-}

module Mean.Parser where

import Control.Applicative (some)
import Control.Monad.Combinators.Expr
    ( Operator(..), makeExprParser, Operator(..) )
import Debug.Trace (traceM)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Debug (dbg)

import Control.Monad.Identity ( runIdentity, Identity )
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text.IO as TiO
import Control.Monad.State
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Mean.Core as Core
import Mean.Viz

newtype ParseState = ParseState {inTree :: Bool}

type Parser = P.ParsecT Void Text (StateT ParseState Identity)

keywords :: [String]
keywords = ["if", "then", "else", "let", "dom", "case", "of"]

lineComment = L.skipLineComment "//"

blockComment = L.skipBlockComment "/*" "*/"

space :: Parser ()
space = L.space C.hspace1 lineComment blockComment

spaceN :: Parser ()
spaceN = L.space C.space1 lineComment blockComment

delimiter = P.some (space >> P.some C.newline >> space)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

btw sym0 sym1 = P.between (symbol sym0) (symbol sym1)

brackets :: Parser a -> Parser a
brackets = btw "[" "]"

parens :: Parser a -> Parser a
parens = btw "(" ")"

angles :: Parser a -> Parser a
angles = btw "<" ">"

curlies :: Parser a -> Parser a
curlies = btw "{" "}"

reserved :: Text -> Parser ()
reserved w = (lexeme . P.try) (C.string w *> P.notFollowedBy C.alphaNumChar)

identifier :: Parser String
identifier = (lexeme . P.try) (p >>= check)
  where
    p = (:) <$> C.letterChar <*> P.many (C.alphaNumChar <|> C.char '\'')
    check x =
      if x `elem` keywords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else pure x

titularIdentifier = P.lookAhead C.upperChar >> identifier

lIdentifier = P.lookAhead C.lowerChar >> identifier

integer :: Parser Integer
integer = lexeme L.decimal

infixOpL, infixOpR :: Text -> (a -> a -> a) -> Operator Parser a
infixOpL name f = InfixL (f <$ symbol name)
infixOpR name f = InfixR (f <$ symbol name)

prefixOp, postfixOp :: Text -> (a -> a) -> Operator Parser a
prefixOp name f = Prefix (f <$ symbol name)
postfixOp name f = Postfix (f <$ symbol name)

indent :: (Show a) => ([b] -> Parser a) -> Parser b -> Parser a
indent p0 p1 = dbg "indent" $ L.indentBlock spaceN p
  where
    p = pure $ L.IndentSome Nothing p0 p1

parse parser i = runIdentity $ evalStateT (P.runParserT parser "<input>" i) (ParseState {inTree = False})

parseFile parser file = TiO.readFile file <&> \i -> evalStateT (P.runParserT parser file i) (ParseState {inTree = False})

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type TyParser = L.Parser S.Type

pTyTerm :: TyParser
pTyTerm =
  P.choice
    [ L.angles pType,
      L.reserved "t" >> pure S.tyBool,
      L.reserved "n" >> pure S.tyInt,
      L.titularIdentifier <&> S.TyVar . S.TV,
      L.identifier <&> S.TyCon
    ]

tyNil :: TyParser
tyNil = pure S.TyNil

pType :: TyParser
pType = makeExprParser pTyTerm tyOps
  where
    tyOps =
      [ [L.infixOpR "," S.TyFun]
      ]

pTypeAssignment :: TyParser
pTypeAssignment = (L.reserved ":" >> pType) <|> tyNil

pOptionalTypeAssignment :: TyParser
pOptionalTypeAssignment = pTypeAssignment <|> tyNil

-------------------------------------------------------------------------------
-- Terms
-------------------------------------------------------------------------------

type CExprParser = L.Parser S.CoreExpr

type LitParser = L.Parser S.Lit

pBool :: LitParser
pBool =
  (L.reserved "True" >> pure (S.LBool True))
    <|> (L.reserved "False" >> pure (S.LBool False))

pInt :: LitParser
pInt = S.LInt . fromIntegral <$> L.integer

pLit :: LitParser
pLit = P.choice [pInt, pBool]

pCLit :: CExprParser
pCLit = S.CLit <$> pLit

pVar :: L.Parser S.Var
pVar = S.mkVar <$> L.identifier

pCVar :: CExprParser
pCVar = S.CVar <$> pVar

pBinder :: L.Parser S.Binder
pBinder = do
  L.symbol "\\"
  n <- L.identifier
  S.Binder (S.mkVar n) <$> pOptionalTypeAssignment

pLam :: L.Parser (a -> S.Lambda a)
pLam = do
  b <- pBinder
  L.symbol "."
  pure $ S.Lam b

pCLam :: CExprParser
pCLam = do
  lam <- pLam
  S.CLam . lam <$> pCExpr

pCond :: L.Parser a -> L.Parser ((S.TernOp -> a -> a -> a -> a) -> a)
pCond pExpr = do
  L.reserved "if"
  x <- pExpr
  L.reserved "then"
  y <- pExpr
  L.reserved "else"
  z <- pExpr
  pure $ \mkC -> mkC S.Cond x y z

pCCond = do
  c <- pCond pCExpr
  pure $ c S.CTernOp

pCTerm :: CExprParser
pCTerm =
  P.choice
    [ L.parens pCExpr,
      pCLit,
      pCVar,
      pCLam,
      pCCond
    ]

cOperatorTable :: [[Operator L.Parser S.CoreExpr]]
cOperatorTable =
  [ [ L.prefixOp "!" (S.CUnOp S.Neg)
    ],
    [ L.infixOpL "==" (S.CBinOp S.Eq),
      L.infixOpL "!=" (S.CBinOp S.NEq),
      L.infixOpL "&&" (S.CBinOp S.And),
      L.infixOpL "||" (S.CBinOp S.Or)
    ]
  ]

pCExpr' :: CExprParser
pCExpr' = makeExprParser pCTerm cOperatorTable

pCExpr :: CExprParser
pCExpr = do
  exprs <- some pCExpr'
  pure (foldl1 (\e0 e1 -> S.CApp $ S.App e0 e1) exprs)

type ExprParser = L.Parser S.SugarExpr

type ExprTreeParser = L.Parser S.ExprTree

pENode = P.try pSExpr <|> pSBind

mkLeafNode e = S.Node e S.Leaf S.Leaf

mkUnNode e l = S.Node e l S.Leaf

pLeafNode :: ExprTreeParser
pLeafNode = L.brackets $ mkLeafNode <$> pENode

pUnNode :: ExprTreeParser
pUnNode = L.brackets $ do
  e <- pENode
  mkUnNode e <$> pTree

pBiNode :: ExprTreeParser
pBiNode = L.brackets $ do
  e <- pENode
  l <- pTree
  S.Node e l <$> pTree

pTree :: ExprTreeParser
pTree =
  P.choice
    [P.try pLeafNode, P.try pUnNode, pBiNode]

pSLit :: ExprParser
pSLit = S.SLit <$> Core.pLit

pSVar :: ExprParser
pSVar = S.SVar <$> Core.pVar

pSCond = do
  c <- Core.pCond pSExpr
  pure $ c S.STernOp

finally fn p = do
  mA <- P.observing p
  case mA of
    Left e -> fn >> P.parseError e
    Right a -> fn >> pure a

pSTree :: ExprParser
pSTree = do
  s <- get
  put (s {L.inTree = True})
  t <- finally (put $ s {L.inTree = False}) pTree
  pure $ S.STree t

pSBind :: ExprParser
pSBind = S.SBind <$> Core.pBinder

pSLam :: ExprParser
pSLam = do
  lam <- Core.pLam
  S.SLam . lam <$> pSExpr

pSet = L.curlies (pSExpr `P.sepBy` (L.space >> C.char ',' >> L.space))

pSSet = S.SSet <$> pSet

pCase = do
  c <- pSExpr
  L.reserved ":"
  r <- pSExpr
  pure (c, r)

pSCase = P.try $ Lex.indentBlock L.spaceN p
  where
    p = do
      L.reserved "case"
      base <- pSExpr
      L.reserved "of"
      pure $ Lex.IndentSome Nothing (pure . S.SCase base) pCase

sTerms =
  [ L.parens pSExpr,
    pSLit,
    pSVar,
    pSLam,
    pSCond,
    pSCase,
    pSSet
  ]

pSTerm :: ExprParser
pSTerm = do
  s <- get
  P.choice $
    [pSTree | not (L.inTree s)] ++ sTerms

sOperatorTable :: [[Operator L.Parser S.SugarExpr]]
sOperatorTable =
  [ [ L.prefixOp "!" (S.SUnOp Core.Neg)
    ],
    [ L.infixOpL "==" (S.SBinOp Core.Eq),
      L.infixOpL "!=" (S.SBinOp Core.NEq),
      L.infixOpL "&&" (S.SBinOp Core.And),
      L.infixOpL "||" (S.SBinOp Core.Or)
    ]
  ]

pSExpr' :: ExprParser
pSExpr' = makeExprParser pSTerm sOperatorTable

pSExpr :: ExprParser
pSExpr = do
  exprs <- some pSExpr'
  pure (foldl1 (\e0 e1 -> S.SApp $ Core.App e0 e1) exprs)

type ExprParser = L.Parser ModuleExpr

type ModuleParse = Either (P.ParseErrorBundle Text Data.Void.Void) Module

pMDecl :: ExprParser
pMDecl = do
  L.reserved "let"
  name <- L.identifier
  L.symbol "="
  MDecl name <$> Sugar.pSExpr

pModule :: L.Parser Module
pModule = pMDecl `P.sepBy` L.delimiter

-- pFModule :: String -> IO ModuleParse
pFModule = parseFile pModule
