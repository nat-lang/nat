module Mean.Viz where

import Mean.Core
import Mean.Sugar
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
  ppr p (Lam b e) =
    ppr p b <> case e of
      CLam Lam {} -> ppr (p + 1) e
      _ -> brackets (ppr (p + 1) e)

instance (Pretty a) => Pretty (App a) where
  ppr p (App e0 e1) = ppr p e0 <> parens (ppr p e1)

instance Pretty Lit where
  ppr p l = case l of
    LInt n -> text (show n)
    LBool b -> text (show b)

ppCond :: (Pretty p1, Pretty p2, Pretty p3) => Int -> p1 -> p2 -> p3 -> Doc
ppCond p x y z = text "if" <+> ppr p x <+> text "then" <+> ppr p y <+> text "else" <+> ppr p z

instance Pretty CoreExpr where
  ppr p e = case e of
    CLit l -> ppr p l
    CVar s -> text (show s)
    CBind b -> ppr p b
    CLam l -> ppr p l
    CApp a -> ppr p a
    CBinOp op e0 e1 -> ppr p e0 <+> text ppOp <+> ppr p e1
      where
        ppOp = case op of
          Eq -> "="
          NEq -> "!="
          And -> "&"
          Or -> "|"
    CUnOp op e -> ppr p e <+> char ppOp
      where
        ppOp = case op of
          Neg -> '¬'
    CTernOp Cond x y z -> ppCond p x y z

instance Pretty TyScheme where
  ppr p (Forall tvs ty) = "Forall" <+> brackets (ppr p tvs) <> ":" <+> ppr p ty

instance Show CoreExpr where
  show e = (show . ppr 0) e

instance Show Type where
  show = show . ppr 0

instance Show TyScheme where
  show = show . ppr 0

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