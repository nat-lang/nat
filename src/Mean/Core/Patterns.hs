{-# LANGUAGE PatternSynonyms #-}
module Mean.Core.Patterns (
  pattern Lam,
  pattern App
) where

import qualified Mean.Core.Syntax as S
import Mean.Core.Syntax (CoreExpr(..), Binder)

pattern Lam :: Binder -> CoreExpr -> CoreExpr
pattern Lam b e <- CLam (S.Lam b e)
pattern App :: CoreExpr -> CoreExpr -> CoreExpr
pattern App e0 e1 <- CApp (S.App e0 e1)

{-# COMPLETE CLit,CVar,Lam,App #-}
