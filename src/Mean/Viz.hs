{-# LANGUAGE ConstrainedClassMethods #-}

module Mean.Viz
  ( text,
    parens,
    module Mean.Viz,
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
