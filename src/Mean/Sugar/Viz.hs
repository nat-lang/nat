{-# LANGUAGE FlexibleInstances #-}

module Mean.Sugar.Viz where
  
import Mean.Common.Viz
import Mean.Core.Viz
import Mean.Core.Syntax
import Mean.Sugar.Syntax
import Text.PrettyPrint
import Prelude hiding ((<>))

instance Pretty (Lambda SugarExpr) where
  ppr p (Lam (Binder n t) e) = char 'λ' <> text (show n) <> case e of
    SLam Lam {} -> ppr (p + 1) e
    _ -> brackets (ppr (p + 1) e)

instance Show ExprNode where
  show (ENode e) = (show . ppr 0) e
  show (BNode b) = (show . ppr 0) b

instance Pretty SugarExpr where
  ppr p e = case e of
    SLit l -> ppr p l
    SVar v -> text $ show v
    SLam l -> ppr p l
    SApp a -> ppr p a
    STree t -> text $ show t

instance Show SugarExpr where
  show = show . ppr 0