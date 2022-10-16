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
    EApp e0 e1 -> fv [e0, e1]
    ECond x y z -> fv [x, y, z]
    EUnOp _ e -> fv e
    EBinOp _ e0 e1 -> fv [e0, e1]
    ETree t -> fv (toList t)
    ETup es -> fv es
    -- ELitCase Expr [(Expr, Expr)]
    -- ESet (Set Expr)
    -- ELet Var Expr Expr
    EFix v e -> fv e Set.\\ Set.singleton v
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
  rename expr = flip walkM expr $ \case
    -- binding contexts
    ELam b e -> uncurry ELam <$> shiftBV b e
    ETyCase e cs -> ETyCase e <$> mapM shiftBP cs
    EFix v e -> rename $ mkFixPoint v e
    -- every fv is renamed
    EVar v | fvOf expr v -> EVar <$> next v
    -- nothing to do
    e -> pure e
    where
      sub' :: Expr -> (Var, Expr) -> Expr
      sub' = flip $ uncurry sub

      shift :: Var -> Expr -> RenameM (Var, Expr)
      shift v e = do
        v' <- next v
        pure (v', sub v (EVar v') e)

      shiftBP :: (Binder Expr, Expr) -> RenameM (Binder Expr, Expr)
      shiftBP (Binder p t, e) = do
        let bv = Set.toList $ fv p

        bv' <- mapM ((pure . EVar) <=< next) bv

        let s = zip bv bv'
        let p' = foldl' sub' p s
        let e' = foldl' sub' e s

        pure (Binder p' t, e')

      shiftBV :: Binder Var -> Expr -> RenameM (Binder Var, Expr)
      shiftBV (Binder v t) e = do
        (v', e') <- shift v e
        pure (Binder v' t, e')
