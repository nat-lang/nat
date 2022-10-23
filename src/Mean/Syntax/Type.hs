{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import Mean.Context
import qualified Mean.Parser as P
import Mean.Syntax.Logic
import Mean.Viz (Pretty (ppr), angles, anglesIf, curlies, parens)
import Mean.Walk
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
  | TyQuant (QExpr Type)
  | TyTyCase Type [(Type, Type)]
  | TyUndef
  | TyWild
  deriving (Prel.Eq, Ord)

instance Walkable Type where
  walkMC' f ty = f ty ctn
    where
      go = walkMC' f
      ctn = \case
        TyFun t0 t1 -> TyFun <$> go t0 <*> go t1
        TyTup ts -> TyTup <$> mapM go ts
        TyUnion ts -> TyUnion . Set.fromList <$> mapM go (Set.toList ts)
        TyQuant (Univ v t) -> TyQuant <$> (Univ v <$> go t)
        TyTyCase v ts -> do
          let (tsL, tsR) = unzip ts
          tsL' <- mapM go tsL
          tsR' <- mapM go tsR
          v' <- go v
          pure $ TyTyCase v' (zip tsL' tsR')
        t' -> f t' pure

mkTv :: String -> Type
mkTv = TyVar . mkVar

mkTyUnion = TyUnion . Set.fromList

tyInt, tyBool :: Type
tyInt = TyCon "n"
tyBool = TyCon "t"

instance Pretty Type where
  ppr p (TyCon t) = text t
  ppr p TyWild = text "_"
  ppr p (TyVar v) = text (show v)
  ppr p (TyFun a b) = ppr (p + 1) a <> text "->" <> ppr (p + 1) b
  ppr p (TyUnion ts) = curlies $ text (intercalate " | " (show <$> Set.toList ts))
  ppr p (TyTup ts) = parens $ text (intercalate ", " (show <$> ts))
  ppr p (TyQuant (Univ tvs ty)) = "Forall" <+> brackets (ppr p tvs) <> ":" <+> ppr p ty
  ppr p TyUndef = text "TyUndef"
  ppr p (TyTyCase v ts) = ppr p v <> text ":" <> text (show ts)

instance Show Type where
  show = show . ppr 0

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

type TyParser = P.Parser Type

pTyTerm :: TyParser
pTyTerm =
  P.choice
    [ P.angles pType,
      P.reserved "t" >> pure tyBool,
      P.reserved "n" >> pure tyInt,
      P.reserved "undef" >> pure TyUndef,
      P.titularIdentifier <&> mkTv,
      P.identifier <&> TyCon,
      P.parens $ P.commaSep pTyTerm <&> TyTup
    ]

pType :: TyParser
pType = P.makeExprParser pTyTerm tyOps
  where
    tyOps =
      [ [P.infixOpR "," TyFun]
      ]

pTypeAssignment :: TyParser
pTypeAssignment = P.reserved ":" >> pType

pOptionalType :: TyParser
pOptionalType = pTypeAssignment P.<|> pure (mkTv "A")
