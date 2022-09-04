{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.Viz where

import Mean.Common.Viz ( Pretty(ppr), angles, anglesIf )
-- import Mean.Core.Evaluation (EvalError(..))
import Mean.Core.Syntax
import Text.PrettyPrint
    ( (<+>), (<>), brackets, char, parens, text )
import Prelude hiding ((<>), Eq)

instance Pretty TyVar where
  ppr _ (TV t) = text t

instance Pretty [TyVar] where
  ppr p (t : ts) = ppr p t <> char ',' <> ppr p ts
  ppr p [] = ""

instance Pretty Type where
  ppr p (TyCon t) = anglesIf (p == 0) $ text t
  ppr p (TyVar t) = anglesIf (p == 0) $ ppr p t
  ppr p (TyFun a b) = angles $ ppr (p + 1) a <> char ',' <> ppr (p + 1) b
  ppr p TyNil = text "TyNil"

instance Pretty Binder where
  ppr p (Binder n t) = char 'λ' <> text (show n)

instance Pretty (Lambda CoreExpr) where
  ppr p (Lam b e) = ppr p b <> case e of
    CLam Lam {} -> ppr (p + 1) e
    _ -> brackets (ppr (p + 1) e)

instance (Pretty a) => Pretty (App a) where
  ppr p (App e0 e1) = ppr p e0 <> parens (ppr p e1)

instance Pretty Lit where
  ppr p l = case l of
    LInt n -> text (show n)
    LBool b -> text (show b)

instance Pretty CoreExpr where
  ppr p e = case e of
    CLit l -> ppr p l
    CVar s -> text (show s)
    CBind b -> ppr p b
    CLam l -> ppr p l
    CApp a -> ppr p a
    CBinOp op e0 e1 -> ppr p e0 <+> char ppOp <+> ppr p e1 where
      ppOp = case op of
        Eq -> '='
        And -> '&'
        Or -> '|'
    CUnOp op e -> ppr p e <+> char ppOp where
      ppOp = case op of
        Neg -> '¬'
    CCond (Cond x y z) -> text "if" <+> ppr p x <+> text "then" <+> ppr p y <+> text "else" <+> ppr p z

instance Pretty TyScheme where
  ppr p (Forall tvs ty) = "Forall" <+> brackets (ppr p tvs) <> ":" <+> ppr p ty

instance Show CoreExpr where
  show e = (show . ppr 0) e

instance Show Type where
  show = show . ppr 0

instance Show TyScheme where
  show = show . ppr 0