{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Type where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Debug.Trace (traceM)
import qualified Mean.Parser as P
import Mean.Syntax.Logic
import Mean.Var
import Mean.Viz (Pretty (ppr), angles, anglesIf, curlies, parens)
import Text.PrettyPrint
  ( Doc,
    brackets,
    char,
    parens,
    text,
    (<+>),
    (<>),
  )
import Prelude hiding (Eq, GT, LT, (*), (<>), (>))
import qualified Prelude as Prel

data Type
  = TyVar Var
  | TyCon String
  | TyFun Type Type
  | TyTup [Type]
  | TyUnion (Set.Set Type)
  | TyUndef
  | TyQuant (QExpr Type)
  | -- TyMap
    -- TyNil is a placeholder left by the parser in lieu of an explicit
    -- type annotation. It says "infer my type, please".
    TyNil
  deriving (Prel.Eq, Ord)

mkTv :: String -> Type
mkTv = TyVar . mkVar

mkTyUnion = TyUnion . Set.fromList

tyInt, tyBool :: Type
tyInt = TyCon "n"
tyBool = TyCon "t"

instance Pretty Type where
  ppr p (TyCon t) = anglesIf (p == 0) $ text t
  ppr p (TyVar v) = anglesIf (p == 0) $ text (show v)
  ppr p (TyFun a b) = angles $ ppr (p + 1) a <> char ',' <> ppr (p + 1) b
  ppr p (TyUnion ts) = curlies $ text (intercalate " | " (show <$> Set.toList ts))
  ppr p (TyTup ts) = parens $ text (intercalate ", " (show <$> ts))
  ppr p (TyQuant (Univ tvs ty)) = "Forall" <+> brackets (ppr p tvs) <> ":" <+> ppr p ty
  ppr p TyNil = text "TyNil"

instance Show Type where
  show = show . ppr 0

-------------------------------------------------------------------------------
-- Parsing (Types)
-------------------------------------------------------------------------------

type TyParser = P.Parser Type

pTyTerm :: TyParser
pTyTerm =
  P.choice
    [ P.angles pType,
      P.reserved "t" >> pure tyBool,
      P.reserved "n" >> pure tyInt,
      P.titularIdentifier <&> mkTv,
      P.identifier <&> TyCon,
      P.parens $ P.commaSep pTyTerm <&> TyTup
    ]

tyNil :: TyParser
tyNil = pure TyNil

pType :: TyParser
pType = P.makeExprParser pTyTerm tyOps
  where
    tyOps =
      [ [P.infixOpR "," TyFun]
      ]

pTypeAssignment :: TyParser
pTypeAssignment = (P.reserved ":" >> pType) P.<|> tyNil

pOptionalType :: TyParser
pOptionalType = pTypeAssignment P.<|> tyNil
