{-# LANGUAGE GADTs, OverloadedStrings #-}

module Mean.Case where

import Mean.Core
import Mean.Relations
import Mean.Viz
import qualified Mean.Parser as P
import Text.PrettyPrint
import Data.List
import Prelude hiding ((<>), (*), (&&), (||))

data CaseExpr where
  Case :: Expressible a => a -> [(a, a)] -> CaseExpr

instance Pretty CaseExpr where
  ppr p e = case e of
    Case c es -> text "case" <+> ppr p c <> char ':' <+> text (intercalate ", " (show . pp <$> es))
      where
        pp (e0, e1) = ppr p e0 <+> text "->" <+> ppr p e1

instance Show CaseExpr where
  show = show . ppr 0

instance Reducible CaseExpr where
  reduce expr = case expr of
    Case b cs -> do
      b' <- reduce b
      case cs of
        [] -> error "empty case statement"
        [(c, e)] -> do
          c' <- reduce c
          b' <- reduce b
          if c' == b'
            then reduce e
            else error "case without default"
        ((c, e) : cs) -> do
          c' <- reduce c
          e' <- reduce e
          cs' <- reduce (Case b cs)
          reduce $ RTernOp Cond (b' === c') e' cs'

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

pCase :: Expressible a => P.Parser a -> P.Parser CaseExpr
pCase pExpr = P.try $ P.indentBlock P.spaceN p
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
      pure $ P.IndentSome Nothing (pure . Case base) pCase
