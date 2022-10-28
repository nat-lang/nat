{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Context where

import Control.Monad (forM, liftM, (<=<))
import Data.Foldable (foldl', toList)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Mean.Context
import Mean.Syntax.Module
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Mean.Unification
import Mean.Walk

-------------------------------------------------------------------------------
-- Type contexts
-------------------------------------------------------------------------------

instance Substitutable Type Type where
  sub v t = walk $ \case
    TyVar v' | v' == v -> t
    t' -> t'

instance Contextual Type where
  fv = \case
    TyVar a -> Set.singleton a
    TyCon {} -> Set.empty
    TyFun t0 t1 -> fv t0 `Set.union` fv t1
    TyTup ts -> foldMap fv ts
    TyTyCase b cs -> let (csL, csR) = unzip cs in Set.unions [fv b, fv csL, fv csR]
    TyUnion ts -> foldMap fv ts
    TyNil -> Set.empty
    TyWild -> Set.empty
    TyUndef -> Set.empty

mkTv' i = let c = 'A' : show i in TyVar (Var c c)

-------------------------------------------------------------------------------
-- Expression contexts
-------------------------------------------------------------------------------

instance Contextual Expr where
  fv = \case
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
    _ -> Set.empty

instance Substitutable Expr Expr where
  sub v e = walkC $ \ctn -> \case
    -- base case
    EVar v' | v' == v -> e
    -- ignore contexts that already bind v
    e'@(ELam (Binder bv t) _) | bv == v -> e'
    ETyCase c cs -> ETyCase (ctn c) (fmap ctn' cs)
      where
        ctn' p@(Binder b t, e) = if fvOf b v then p else (Binder b t, ctn e)
    -- otherwise continue the walk
    e' -> ctn e'

shift :: Var -> Expr -> FreshM (Var, Expr)
shift v e = do
  ev' <- next v
  let (EVar v') = ev'
  pure (v', sub v ev' e)

shiftBP :: (Binder Expr, Expr) -> FreshM (Binder Expr, Expr)
shiftBP (Binder p t, e) = do
  let bv = Set.toList $ fv p

  bv' <- mapM (next :: Var -> FreshM Expr) bv

  let s = Map.fromList $ zip bv bv'
  let [p', e'] = inEnv s <$> [p, e]

  pure (Binder p' t, e')

instance Renamable Expr where
  next v = EVar <$> next' v

  rename' vs expr = flip walkM expr $ \case
    -- base case
    EVar v | Set.member v vs -> next v
    -- binding contexts
    ELam (Binder v t) e -> do
      (v', e') <- shift v e
      pure (ELam (Binder v' t) e')
    ETyCase e cs -> ETyCase e <$> mapM shiftBP cs
    EFix v e -> uncurry EFix <$> shift v e
    -- nothing to do
    e -> pure e

instance AlphaComparable Expr where
  (@=) e0 e1 = evalRename e0 == evalRename e1

-------------------------------------------------------------------------------
-- Explicit type contexts
-------------------------------------------------------------------------------

-- take care to preserve the syntactic identity of variables
-- within the scopes of typed lambdas and tycase patterns
renameETypes :: Expr -> FreshM Expr
renameETypes = walkM $ \case
  ELam b e -> do
    b' <- renameB b
    pure $ ELam b e
  ETyCase c cs -> ETyCase c <$> mapM m' cs
    where
      m' (b, e) = do
        b' <- renameB b
        pure (b', e)
  e' -> pure e'
  where
    renameB (Binder v t) = do
      let fvs = Set.toList $ fv t
      tvs <- mapM (fmap TyVar . next') fvs
      let sub = Map.fromList (zip fvs tvs)
      let t' = inEnv sub t
      pure (Binder v t')

-------------------------------------------------------------------------------
-- Module contexts
-------------------------------------------------------------------------------

instance Contextual ModuleExpr where
  fv = \case
    MDecl _ e -> fv e
    MLetRec v e -> fv e Set.\\ Set.singleton v
    MExec e -> fv e
  bv = \case
    MDecl v _ -> Set.singleton v
    MLetRec v _ -> Set.singleton v
    _ -> Set.empty

instance {-# OVERLAPPING #-} Contextual Module where
  fv mod = unfoldSet fv mod Set.\\ bv mod
  bv = unfoldSet bv

instance Substitutable Expr ModuleExpr where
  sub v expr mExpr =
    let sub' = sub v expr
     in case mExpr of
          MDecl v e -> MDecl v (sub' e)
          MLetRec v' e | v /= v' -> MLetRec v' (sub' e)
          MExec e -> MExec (sub' e)
          _ -> mExpr

instance Renamable ModuleExpr where
  rename' vs mExpr = case mExpr of
    MDecl v e -> MDecl v <$> rename' vs e
    MLetRec v e -> MLetRec v <$> rename' vs e
    MExec e -> MExec <$> rename' vs e

instance Renamable Module where
  rename' vs mod = (thdPass <=< sndPass <=< fstPass) mod
    where
      reBoundEnv mod = Map.fromList $ [(reset v, EVar v) | v <- Set.toList $ bv mod]

      -- rename topmost let vars
      fstPass :: Module -> FreshM Module
      fstPass mod = forM mod $ \case
        MDecl v e -> MDecl <$> next' v <*> pure e
        MLetRec v e -> MLetRec <$> next' v <*> pure e
        mExpr -> pure mExpr

      -- update topmost let vars bound in every expr
      sndPass :: Module -> FreshM Module
      sndPass mod = pure $ inEnv (reBoundEnv mod) <$> mod

      -- now rename each expr, ignoring the bound let vars
      thdPass :: Module -> FreshM Module
      thdPass mod =
        let rename e = rename' vs e
         in mapM rename mod

  evalRename = evalRename' . renameTypes . evalRename' . rename
    where
      renameTypes mod = forM mod (mapM renameETypes)
