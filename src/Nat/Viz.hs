{-# LANGUAGE ConstrainedClassMethods #-}

module Nat.Viz
  ( text,
    parens,
    char,
    module Nat.Viz,
  )
where

import Text.PrettyPrint
  ( Doc,
    char,
    parens,
    text,
    (<+>),
    (<>),
  )
import Prelude hiding (Eq, (<>))

class Pretty a where
  ppr :: Int -> a -> Doc

  pp :: a -> Doc
  pp = ppr 0

p :: Show a => a -> Doc
p = text . show

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
