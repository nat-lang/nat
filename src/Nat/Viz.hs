{-# LANGUAGE ConstrainedClassMethods #-}

module Nat.Viz
  ( text,
    parens,
    module Nat.Viz,
  )
where

import Data.List (intercalate)
import Text.PrettyPrint
  ( Doc,
    char,
    hcat,
    parens,
    punctuate,
    text,
    (<+>),
    (<>),
  )
import Prelude hiding (Eq, (<>))

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

brackets :: Doc -> Doc
brackets p = char '[' <> p <> char ']'

curlies :: Doc -> Doc
curlies p = char '{' <> p <> char '}'

fnIf :: (a -> a) -> Bool -> a -> a
fnIf fn b = if b then fn else id

anglesIf :: Bool -> Doc -> Doc
anglesIf = fnIf angles

parensIf :: Bool -> Doc -> Doc
parensIf = fnIf parens

bracketsIf :: Bool -> Doc -> Doc
bracketsIf = fnIf brackets

sep :: String -> [Doc] -> Doc
sep punc = hcat . punctuate (text punc)

prettyPair :: (Pretty p1, Pretty p2) => String -> (p1, p2) -> Doc
prettyPair delim (a, b) = text "\t" <> pp a <+> text delim <+> pp b

prettyPairs :: (Pretty p1, Pretty p2) => String -> [(p1, p2)] -> Doc
prettyPairs delim = sep "\n" . fmap (prettyPair delim)