{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Syntax.Type where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Nat.Context
import qualified Nat.Parser as P
import Nat.Viz (Pretty (ppr), angles, anglesIf, curlies, parens)
import Nat.Walk
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
  | TyCon Var
  | TyFun Type Type
  | TyTup [Type]
  | TyUnion (Set.Set Type)
  | TyTyCase Type [(Type, Type)]
  | -- TyNil is a placeholder left by the parser and factory functions in
    -- lieu of an explicit type annotation. It says "make me a fresh tv"
    TyNil
  | TyUndef
  | TyWild
  deriving (Prel.Eq, Ord)

instance Walkable Type where
  walkMC' f ty = f ctn ty
    where
      go = walkMC' f
      ctn = \case
        TyFun t0 t1 -> TyFun <$> go t0 <*> go t1
        TyTup ts -> TyTup <$> mapM go ts
        TyUnion ts -> TyUnion . Set.fromList <$> mapM go (Set.toList ts)
        TyTyCase v ts -> do
          let (tsL, tsR) = unzip ts
          tsL' <- mapM go tsL
          tsR' <- mapM go tsR
          v' <- go v
          pure $ TyTyCase v' (zip tsL' tsR')
        t' -> f pure t'

mkTv :: String -> Type
mkTv = TyVar . mkVar

mkTyUnion = TyUnion . Set.fromList

tyInt, tyBool :: Type
tyInt = TyCon (mkVar "n")
tyBool = TyCon (mkVar "t")

instance Pretty Type where
  ppr p (TyCon v) = text (show v)
  ppr p (TyVar v) = text (show v)
  ppr p (TyFun a b) = ppr (p + 1) a <> text "->" <> ppr (p + 1) b
  ppr p (TyUnion ts) = curlies $ text (intercalate " | " (show <$> Set.toList ts))
  ppr p (TyTup ts) = parens $ text (intercalate ", " (show <$> ts))
  ppr p (TyTyCase v ts) = ppr p v <> text ":" <> text (show ts)
  ppr p TyUndef = text "TyUndef"
  ppr p TyNil = text "TyNil"
  ppr p TyWild = text "_"

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
      pVar <&> TyCon,
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
pTypeAssignment = P.reserved ":" >> pType

pOptionalType :: TyParser
pOptionalType = P.try pTypeAssignment P.<|> tyNil