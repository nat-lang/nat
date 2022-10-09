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
    -- EFix Var Expr
    _ -> Set.empty

instance Substitutable Expr Expr where
  substitute env e = foldl' (\e' s -> walk (uncurry sub s) e') e (Map.toList env)
    where
      sub :: Var -> Expr -> Expr -> Expr
      sub v e e' = case e' of
        EVar v' | v' == v -> e
        _ -> e'

walkFreeM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
walkFreeM f expr =
  trace "??" $
    let go = walkFreeM f
     in f expr >>= (\e -> do traceM $ "1" ++ show e; return e) >>= \case
          EApp e0 e1 -> do
            e0' <- go e0
            e1' <- go e1
            pure (EApp e0' e1')
          ECond x y z -> do
            x' <- go x
            y' <- go y
            z' <- go z
            pure (ECond x' y' z')
          EUnOp op e -> do
            e' <- go e
            pure (EUnOp op e')
          EBinOp op e0 e1 -> do
            e0' <- go e0
            e1' <- go e1
            pure (EBinOp op e0' e1')
          ETree t -> do
            t' <- mapM go t
            pure $ ETree t'
          ELitCase e cs -> do
            e' <- go e
            cs' <- mapM (mapM go) cs
            pure $ ELitCase e' cs'
          ESet es -> do
            es' <- mapM go (Set.toList es)
            pure $ ESet (Set.fromList es')
          ETup es -> do
            es' <- mapM go es
            pure $ ETup es'
          _ -> pure expr

instance Renamable Expr where
  rename = trace "?" $
    walkFreeM $ \e -> trace ("0" ++ show e) $ case e of
      _ -> do
        traceM $ "bar?" ++ show e
        pure (mkEVar "a" ~> (mkEVar "b"))
      -- binding contexts
      ELam b e -> do
        traceM "???"
        (b', e') <- shiftBV b e
        traceM (show (b', e'))
        pure (ELam b' e')
      ETyCase e cs -> do
        cs' <- mapM (uncurry shiftBP) cs
        pure (ETyCase e cs')
      EFix v e -> do
        (v', e') <- shift v e
        pure (EFix v' e')
      -- base case
      EVar v -> next v <&> EVar
      -- nothing to do
      e -> trace "foo?" $ pure e
    where
      -- simply ignore contexts that already bind v
      safesub :: Var -> Expr -> Expr -> Expr
      safesub v e' = walk $ \case
        ELam (Binder bv t) _ | bv == v -> e'
        -- ETyCase e cs -> ETyCase ()
        -- EFix v e ->
        e -> inEnv v e' e

      safesub' :: Expr -> (Var, Expr) -> Expr
      safesub' = flip $ uncurry safesub

      shift v e = do
        v' <- next v
        pure (v', safesub v (EVar v') e)

      shiftBP :: Binder Expr -> Expr -> RenameM (Binder Expr, Expr)
      shiftBP (Binder p t) e = do
        let bv = Set.toList $ fv p

        bv' <- mapM ((pure . EVar) <=< next) bv

        let s = zip bv bv'
        let p' = foldl' safesub' p s
        let e' = foldl' safesub' e s

        pure (Binder p' t, e')

      shiftBV :: Binder Var -> Expr -> RenameM (Binder Var, Expr)
      shiftBV (Binder v t) e = do
        (v', e') <- shift v e
        pure (Binder v' t, e')
