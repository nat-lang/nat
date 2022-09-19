{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Type where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Debug.Trace (traceM)
import qualified Mean.Parser as P
import Mean.Viz (Pretty (ppr), angles, anglesIf)
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

newtype TyVar = TV String
  deriving (Show, Prel.Eq, Ord)

data Type
  = TyVar TyVar
  | TyCon String
  | TyFun Type Type
  | TyUnion (Set.Set Type)
  | -- TyNil is a placeholder left by the parser in lieu of an explicit
    -- type annotation. It says "infer my type, please".
    TyNil
  deriving (Prel.Eq, Ord)

data TyScheme = Forall [TyVar] Type
  deriving (Prel.Eq, Ord)

mkUnqScheme :: Type -> TyScheme
mkUnqScheme = Forall []

mkTv :: String -> Type
mkTv = TyVar . TV

tyInt, tyBool :: Type
tyInt = TyCon "n"
tyBool = TyCon "t"

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

instance Pretty TyScheme where
  ppr p (Forall tvs ty) = "Forall" <+> brackets (ppr p tvs) <> ":" <+> ppr p ty

instance Show Type where
  show = show . ppr 0

instance Show TyScheme where
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
      P.titularIdentifier <&> TyVar . TV,
      P.identifier <&> TyCon
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
