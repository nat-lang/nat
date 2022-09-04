{-# LANGUAGE PatternSynonyms #-}

module Mean.Core.Patterns
  ( pattern Lam,
    pattern App,
    pattern CTrue,
    pattern CFalse
  )
where

import Mean.Core.Syntax (Binder, CoreExpr (..), Lit(..))
import qualified Mean.Core.Syntax as S

pattern Lam :: Binder -> CoreExpr -> CoreExpr
pattern Lam b e <- CLam (S.Lam b e)

pattern App :: CoreExpr -> CoreExpr -> CoreExpr
pattern App e0 e1 <- CApp (S.App e0 e1)

{-# COMPLETE CLit, CVar, CBind, Lam, App #-}

pattern CTrue :: CoreExpr
pattern CTrue <- CLit (LBool True)

pattern CFalse :: CoreExpr
pattern CFalse <- CLit (LBool False)
