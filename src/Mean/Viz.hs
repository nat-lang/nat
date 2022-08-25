{-# LANGUAGE OverloadedStrings #-}

module Mean.Viz where

import Mean.Syntax
import Mean.Evaluation
import Text.PrettyPrint
import Prelude hiding ((<>))

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

fnIf fn b = if b then fn else id

anglesIf = fnIf angles

parensIf = fnIf parens

instance Pretty TyVar where
  ppr _ (TV t) = text t

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
    Var s -> text s
    Lam (Binder n t) e ->
      parensIf (p > 0) $
        char 'λ' <> text n <> char '.' <> ppr (p + 1) e
    App e0 e1 -> ppr p e0 <> parens (ppr p e1)
    Tree t -> text $ show t
    Let n e -> text n <+> char '=' <+> ppr 0 e

instance Pretty TyScheme where
  ppr p (Forall tvs ty) = ppr p ty

instance Show EvalError where
  show (UnboundVariable n) = "Unbound variable: " ++ show n
  show (NotAFn e0 e1) = "Can't apply " ++ show e0 ++ " to " ++ show e1

instance Show Expr where
  show (Tree et) = show et
  show e = (show . ppr 0) e

instance Show Type where
  show = show . ppr 0

instance Show TyScheme where
  show = show . ppr 0