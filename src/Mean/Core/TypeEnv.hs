module Mean.Core.TypeEnv where

import qualified Data.Map as Map
import Data.Monoid
import qualified Mean.Core.Syntax as S

newtype TyEnv = TyEnv {types :: Map.Map S.Name S.TyScheme}
  deriving (Eq)

empty :: TyEnv
empty = TyEnv Map.empty

extend :: TyEnv -> (S.Name, S.TyScheme) -> TyEnv
extend env (x, s) = env {types = Map.insert x s (types env)}

remove :: TyEnv -> S.Name -> TyEnv
remove (TyEnv env) var = TyEnv (Map.delete var env)

merge :: TyEnv -> TyEnv -> TyEnv
merge (TyEnv a) (TyEnv b) = TyEnv (Map.union a b)

toList :: TyEnv -> [(S.Name, S.TyScheme)]
toList (TyEnv env) = Map.toList env

isIn :: S.TyScheme -> TyEnv -> Bool
isIn ty = elem ty . map snd . toList

instance Semigroup TyEnv where
  (<>) = merge

instance Monoid TyEnv where
  mempty = empty
