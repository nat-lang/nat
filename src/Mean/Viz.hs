module Mean.Viz where

import Prelude hiding (Eq, (<>))
import Text.PrettyPrint
    ( Doc, (<+>), (<>), brackets, char, parens, text )
import Prelude hiding ((<>))

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

brackets' :: Doc -> Doc
brackets' p = char '[' <> p <> char ']'

fnIf :: (a -> a) -> Bool -> a -> a
fnIf fn b = if b then fn else id

anglesIf :: Bool -> Doc -> Doc
anglesIf = fnIf angles

parensIf :: Bool -> Doc -> Doc
parensIf = fnIf parens

bracketsIf :: Bool -> Doc -> Doc
bracketsIf = fnIf brackets'
