{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Context where

import Control.Monad (liftM, (<=<))
import Data.Foldable (foldl', toList)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Mean.Context
import Mean.Syntax.Surface
import Mean.Unification
import Mean.Walk

instance Contextual Expr where
  fv expr = case expr of
    EVar v -> Set.singleton v
    ELam (Binder v _) body -> fv body Set.\\ Set.singleton v
    ETyCase e cs ->
      let fvCs = foldl1 Set.union (map (\(Binder p _, e) -> fv e Set.\\ fv p) cs)
       in fv e `Set.union` fvCs
    EFix v e -> fv e Set.\\ Set.singleton v
    EApp e0 e1 -> fv [e0, e1]
    ECond x y z -> fv [x, y, z]
    EUnOp _ e -> fv e
    EBinOp _ e0 e1 -> fv [e0, e1]
    ETree t -> fv (toList t)
    ETup es -> fv es
    ELitCase e es -> fv e `Set.union` fv es
    ESet es -> fv (toList es)
    -- ELet Var Expr Expr
    _ -> Set.empty

instance Substitutable Expr Expr where
  sub v e = walkC $ \e' ctn -> case e' of
    -- base
    EVar v' | v' == v -> e
    -- ignore contexts that already bind v
    ELam (Binder bv t) _ | bv == v -> e'
    ETyCase c cs -> ETyCase (ctn c) (fmap ctn' cs)
      where
        ctn' p@(Binder b t, e) = if fvOf b v then p else (Binder b t, ctn e)
    -- otherwise continue the walk
    _ -> ctn e'

instance Renamable Expr where
  rename' vs expr = flip walkM expr $ \case
    -- binding contexts
    ELam (Binder v t) e -> do
      (v', e') <- shift v e
      pure (ELam (Binder v' t) e')
    ETyCase e cs -> ETyCase e <$> mapM shiftBP cs
    EFix v e -> do
      (v', e') <- shift v e
      pure (EFix v' e')
    -- every fv is renamed
    EVar v | Set.member v vs -> EVar <$> next v
    -- nothing to do
    e -> pure e

shift :: Var -> Expr -> RenameM (Var, Expr)
shift v e = do
  v' <- next v
  pure (v', sub v (EVar v') e)

shiftBP :: (Binder Expr, Expr) -> RenameM (Binder Expr, Expr)
shiftBP (Binder p t, e) = do
  let bv = Set.toList $ fv p

  bv' <- mapM ((pure . EVar) <=< next) bv

  let s = Map.fromList $ zip bv bv'
  let [p', e'] = inEnv s <$> [p, e]

  pure (Binder p' t, e')
