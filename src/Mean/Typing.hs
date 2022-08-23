module Mean.Typing
  ( Type (..),
    TyVar (..)
  )
where

newtype TyVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TyVar TyVar
  | TyCon String
  | TyFun Type Type
  deriving (Eq, Ord)