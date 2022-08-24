{-# LANGUAGE OverloadedStrings #-}

module Mean.Viz where

import Mean.Syntax
import Mean.Typing
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

{-
    Syn.EBinder b -> (char 'λ') <> ppr 0 b
    Syn.App a b -> parensIf (p > 0) ((ppr (p + 1) a) <+> (ppr p b))
    Syn.Lam b e -> pClosure (char 'λ') b e

    instance Pretty Syn.Binder where
      ppr _ (Syn.Binder n t) = text n <+> text "→"
-}

instance Pretty TyVar where
  ppr _ (TV t) = text t

instance Pretty Type where
  ppr p (TyCon t) = anglesIf (p == 0) $ text t
  ppr p (TyVar t) = anglesIf (p == 0) $ ppr p t
  ppr p (TyFun a b) = angles $ ppr (p + 1) a <> char ',' <> ppr (p + 1) b
  ppr p TyNil = text "TyNil"

instance Pretty Expr where
  ppr p e = case e of
    Var s -> text s
    Lam (Binder n t) e -> text n <> char ':' <> ppr 0 t <> char '.' <> ppr (p + 1) e
    App e0 e1 -> ppr p e0 <> parens (ppr p e1)

instance Show Expr where
  show (Tree et) = show et
  show e = (show . ppr 0) e

instance Show Type where
  show = show . ppr 0