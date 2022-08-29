{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.Viz where

import Mean.Core.Evaluation
import Mean.Core.Syntax
import Text.PrettyPrint
import Prelude hiding ((<>))

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

brackets' :: Doc -> Doc
brackets' p = char '[' <> p <> char ']'

fnIf fn b = if b then fn else id

anglesIf = fnIf angles

parensIf = fnIf parens

bracketsIf = fnIf brackets'

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

instance Pretty Expr where
  ppr p e = case e of
    ELit lit -> case lit of
      LInt n -> text (show n)
      LBool b -> text (show b)
    EVar s -> text (show s)
    Lam (Binder n t) e ->
      char 'λ' <> text (show n) <> case e of
        Lam {} -> ppr (p + 1) e
        _ -> brackets (ppr (p + 1) e)
    App e0 e1 -> ppr p e0 <> parens (ppr p e1)

instance Pretty TyScheme where
  ppr p (Forall tvs ty) = "Forall" <+> brackets (ppr p tvs) <> ":" <+> ppr p ty

instance Show EvalError where
  show (UnboundVariable n) = "Unbound variable: " ++ show n
  show (NotAFn e0 e1) = "Can't apply " ++ show e0 ++ " to " ++ show e1

instance Show Expr where
  show e = (show . ppr 0) e

instance Show Type where
  show = show . ppr 0

instance Show TyScheme where
  show = show . ppr 0