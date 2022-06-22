{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Service.Serializers where

import Control.Applicative ((<|>))
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Vector ((!))

import qualified Interpreter.Fragment as F
import qualified Interpreter.Compose as C
import qualified Compiler.Tree.Syntax as T
import qualified Compiler.Tree.Parser as P

import Compiler.Core.Pretty

{-
parseConstituencyNode :: Object -> Parser T.ConstituencyTree
parseConstituencyNode obj = do
  label <- obj .: "label"
  pos <- obj .: "id"
  cs <- obj .:? "children"
  (c1, c2) <- parseConstituencyChildren cs
  pure $ T.Node (T.CLabel label pos) c1 c2

parseConstituencyChildren :: Maybe Array -> Parser (T.ConstituencyTree, T.ConstituencyTree)
parseConstituencyChildren Nothing = pure $ (T.Leaf, T.Leaf)
parseConstituencyChildren (Just cs) = case length cs of
  0 -> pure $ (T.Leaf, T.Leaf)
  1 -> pure $ (parseChild 0, T.Leaf)
  2 -> pure $ (parseChild 0, parseChild 1)
  _ -> fail "must be binary tree"
  where
    parseChild :: Int -> T.ConstituencyTree
    parseChild i = case ((parseMaybe parseJSON $ cs!i) :: Maybe T.ConstituencyTree) of
      Nothing -> T.Leaf
      Just s  -> s
-}

parseConstituencyNode :: Object -> Parser T.ConstituencyTree
parseConstituencyNode obj = do
  parseString <- obj .: "constituencyParse"
  case P.parseConstituencyTree parseString  of
    Left err -> fail $ show err
    Right tree -> pure tree

instance FromJSON T.ConstituencyTree where
  parseJSON = withObject "SyntaxTree" parseConstituencyNode

instance ToJSON C.SemanticTree where
  toJSON (T.Node s c1 c2) = object $ (serializeSemLabel s) <> [ "children" .= (map toJSON (filter isNode [c1,c2])) ]
    where
      isNode :: C.SemanticTree -> Bool
      isNode s = case s of
        T.Leaf -> False
        _ -> True
      
      serializeSemLabel :: C.SemanticLabel -> [Pair]
      serializeSemLabel (sl, el, cl) = (serializeEvaluationLabel sl)
                                    <> (serializeTypeCheckedExprLabel el)
                                    <> (serializeConstituencyLabel cl)

      serializeEvaluationLabel :: Maybe C.EvaluatedExpr -> [Pair]
      serializeEvaluationLabel sl = case sl of
        Nothing -> [ "value" .= Null, "valuationError" .= Null ]
        Just (Right v) -> [ "value" .= show v, "valuationError" .= Null ]
        Just (Left err) -> [ "value" .= Null, "valuationError" .= show err ]
                            
      serializeTypeCheckedExprLabel :: Maybe C.TypeCheckedExpr -> [Pair]
      serializeTypeCheckedExprLabel el = case el of
        Nothing -> [ "expr" .= Null, "type" .= Null, "typeError" .= Null  ]
        Just te -> case te of
          C.TypedExpr e t -> [ "expr" .= show e, "type" .= show t, "typeError" .= Null ]
          C.UntypedExpr e err -> [ "expr" .= show e, "type" .= Null, "typeError" .= show err ]

      serializeConstituencyLabel :: T.ConstituencyLabel -> [Pair]
      serializeConstituencyLabel (T.CLabel l pos) = ["syntaxLabel" .= (cLabel l), "id" .= pos ]

      cLabel l = case l of
        T.CatLabel l' -> l'
        T.LexLabel l' -> l'

instance ToJSON T.Label where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON T.ConstituencyLabel where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON T.ConstituencyTree where
  toEncoding = genericToEncoding defaultOptions