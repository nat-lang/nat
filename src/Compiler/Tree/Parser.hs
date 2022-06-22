module Compiler.Tree.Parser (
  parseConstituencyTree,
  parseUnPosConstituencyTree
) where

import Debug.Trace (trace, traceM)
import Data.Foldable                 (toList)
import Data.Char                     (digitToInt)
import Data.List                     (foldl')
import           Control.Monad.State
import           Text.Megaparsec     ((<|>))
import qualified Text.Megaparsec  as P

import Compiler.Tree.Lexer        as L
import Compiler.Tree.Syntax       as S

type CTreeParser = L.Parser (S.Tree S.Label)

mkLexNode lex = S.Node (S.LexLabel lex) S.Leaf S.Leaf
mkBinaryCatNode cat l r = S.Node (S.CatLabel cat) l r
mkUnaryCatNode cat l = S.Node (S.CatLabel cat) l S.Leaf

lexNode :: CTreeParser
lexNode = do
  label <- identifier
  pure $ mkLexNode label

cTree' :: (CTreeParser -> CTreeParser) -> CTreeParser
cTree' delimit  = P.try (delimit unaryCatNode) <|> (delimit binaryCatNode)
  where
    node = P.try lexNode <|> P.try (delimit lexNode) <|> cTree' delimit

    unaryCatNode :: CTreeParser
    unaryCatNode = do
      cat <- identifier
      l   <- node
      pure $ mkUnaryCatNode cat l

    binaryCatNode :: CTreeParser
    binaryCatNode = do
      cat <- identifier
      l   <- node
      r   <- node
      pure $ mkBinaryCatNode cat l r

cTree = P.try (cTree' brackets) <|> (cTree' parens)

positionCTree :: S.Tree S.Label -> S.ConstituencyTree
positionCTree t = go "" t
  where
    go pos (S.Node label l r) = S.Node (CLabel label pos) (go (pos ++ "0") l) (go (pos ++ "1") r)
    go _ S.Leaf = S.Leaf

parseUnPosConstituencyTree s = P.runParser cTree "<input>" s

parseConstituencyTree s = case P.runParser cTree "<input>" s of
  Right tree -> Right $ positionCTree tree
  Left err -> Left err
