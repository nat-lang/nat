{-# LANGUAGE FlexibleInstances #-}

module Mean.Evaluation.Module where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import qualified Data.Map as Map
import Debug.Trace (trace)
import Mean.Evaluation.Surface
import Mean.Syntax.Module
import Mean.Syntax.Surface
import Mean.Unification

instance Reducible Module where
  reduce mod = reduce' mod []
    where
      reduce' :: Module -> Module -> Evaluation Module
      reduce' mod mod' =
        trace ("\n??=>> " ++ show mod) $
          let reduceIn = reduce . substitute (Map.fromList [(v, e) | MDecl v e <- mod'])
           in case mod of
                [] -> pure mod'
                (e : es) ->
                  let next m = reduce' es (mod' ++ [m])
                   in case e of
                        MDecl v e -> next <=< (pure . MDecl v) <=< reduceIn $ e
                        MExec e -> next <=< (pure . MExec) <=< reduceIn $ e

eval :: Module -> Either EvalError Module
eval m = runIdentity $ runExceptT $ reduce m
