module Compiler.Tree.Parser
  ( parseConstituencyTree,
    parseUnPosConstituencyTree,
  )
where

import Compiler.Tree.Lexer as L
import Compiler.Tree.Syntax as S
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (foldl')
import Debug.Trace (trace, traceM)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P

type CTreeParser = L.Parser (S.Tree S.Label)

mkLexNode lex = S.Node (S.LexLabel lex) S.Leaf S.Leaf

mkBinaryCatNode cat = S.Node (S.CatLabel cat)

mkUnaryCatNode cat l = S.Node (S.CatLabel cat) l S.Leaf

lexNode :: CTreeParser
lexNode = do mkLexNode <$> identifier

cTree' :: (CTreeParser -> CTreeParser) -> CTreeParser
cTree' delimit = P.try (delimit unaryCatNode) <|> delimit binaryCatNode
  where
    node = P.try lexNode <|> P.try (delimit lexNode) <|> cTree' delimit

    unaryCatNode :: CTreeParser
    unaryCatNode = do
      cat <- identifier
      l <- node
      pure $ mkUnaryCatNode cat l

    binaryCatNode :: CTreeParser
    binaryCatNode = do
      cat <- identifier
      l <- node
      r <- node
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
