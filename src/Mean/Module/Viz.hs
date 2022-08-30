module Mean.Module.Viz where

import Mean.Common.Viz
import Mean.Sugar.Viz
import Mean.Module.Syntax
import Text.PrettyPrint
import Prelude hiding ((<>))

instance Pretty ModuleExpr where
  ppr p e = case e of
    MDecl n e -> text "let" <+> text (show n) <+> char '=' <+> ppr p e
    
instance Show ModuleExpr where
  show = show . ppr 0