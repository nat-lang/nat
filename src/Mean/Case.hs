{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances #-}

module Mean.Case where

import Mean.Core
import Text.PrettyPrint
import Prelude hiding ((<>), (*), (&&), (||))


data CaseExpr where
  Case :: Reducible a => a -> [(a, a)] -> CaseExpr

instance Reducible CaseExpr where
  reduce expr = case expr of
    Case b cs -> do
      b' <- reduce b
      case cs of
        [] -> error "empty case statement"
        [(c, e)] -> do
          c' <- reduce c
          b' <- reduce b
          if c' == b'
            then reduce e
            else error "case without default"
        ((c, e) : cs) -> do
          c' <- reduce c
          e' <- reduce e
          cs' <- reduce (Case b cs)
          pure $ CTernOp Cond (b' === c') e' cs'