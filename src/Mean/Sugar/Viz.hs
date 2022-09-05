{-# LANGUAGE FlexibleInstances #-}

module Mean.Sugar.Viz where

import Data.List
import Mean.Common.Viz
import Mean.Core.Syntax
import Mean.Core.Viz
import Mean.Sugar.Syntax
import Text.PrettyPrint
import Prelude hiding ((<>))

instance Pretty (Lambda SugarExpr) where
  ppr p (Lam (Binder n t) e) =
    char 'λ' <> text (show n) <> case e of
      SLam Lam {} -> ppr (p + 1) e
      _ -> brackets (ppr (p + 1) e)

instance Pretty SugarExpr where
  ppr p e = case e of
    SLit l -> ppr p l
    SVar v -> text $ show v
    SBind b -> ppr p b
    SLam l -> ppr p l
    STernOp Cond x y z -> ppCond p x y z
    SApp a -> ppr p a
    STree t -> text $ drawTree t
    SCase c es -> text "case" <+> ppr p c <> char ':' <+> text (intercalate ", " (show . pp <$> es))
      where
        pp (e0, e1) = ppr p e0 <+> text "->" <+> ppr p e1
    SSet es -> brackets $ text (intercalate ", " (show . ppr p <$> es))

instance Show SugarExpr where
  show = show . ppr 0