{-# LANGUAGE PatternSynonyms #-}

module Mean.Sugar.Patterns
  ( pattern STrue,
    pattern SFalse
  )
where

import Mean.Core.Syntax
import Mean.Sugar.Syntax

pattern STrue :: SugarExpr
pattern STrue <- SLit (LBool True)

pattern SFalse :: SugarExpr
pattern SFalse <- SLit (LBool False)
