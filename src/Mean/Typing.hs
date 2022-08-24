module Mean.Typing where

newtype TyVar = TV String
  deriving (Show, Eq, Ord)

tyInt, tyBool :: Type
tyInt = TyCon "n"
tyBool = TyCon "t"

data Type
  = TyVar TyVar
  | TyCon String
  | TyFun Type Type
  | TyNil
  deriving (Eq, Ord)