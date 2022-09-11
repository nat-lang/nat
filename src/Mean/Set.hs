{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances #-}

module Mean.Set where

import Mean.Core hiding (fresh)
import Mean.Case
import Mean.Viz
import Data.List
import qualified Mean.Parser as P
import qualified Data.Set as Set

data SetExpr where
  Set :: Expressible a => [a] -> SetExpr
  -- SSetComp :: Reducible a => (a, a) -> SetExpr

instance Pretty SetExpr where
  ppr p e = case e of
    Set es -> brackets $ text (intercalate ", " (show . ppr p <$> es))
  
fresh :: [CoreExpr] -> CoreExpr
fresh es = go (mkVar "x") (fv es)
  where
    go v@(Var _ vPri) fv = if Set.member vPri fv
                            then go (incrVarId v) fv
                            else CVar v

instance Reducible SetExpr where
  reduce expr = case expr of
    Set es -> do
      es' <- mapM reduce es
      let x = fresh es'

      e <- reduce $ Case x $ [(c, true) | c <- es'] ++ [(x, false)]
  
      pure (x ~> e)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

pSet pExpr = Set <$> pSet
  where
    pSet = P.curlies (pExpr `P.sepBy` (P.space >> P.char ',' >> P.space))