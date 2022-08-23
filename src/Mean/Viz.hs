module Mean.Viz where

import Mean.Syntax
import Text.PrettyPrint
import Prelude hiding ((<>))

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

instance Pretty Expr where
  ppr p e = case e of
    Var s -> text s

instance Show Expr where
  show = show . ppr 0