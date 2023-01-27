{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Evaluation.Module where

import Control.Monad (foldM, forM, (<=<))
import Control.Monad.Except (Except, ExceptT, liftEither, runExcept, runExceptT, throwError, withExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask)
import Control.Monad.State (evalStateT)
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.List (partition)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Nat.Context
import Nat.Control (mapAccumM)
import Nat.Evaluation.Context
import Nat.Evaluation.Surface
import Nat.Evaluation.Type
import Nat.Inference
import Nat.Reduction
import Nat.Syntax.Module
import Nat.Syntax.Surface
import Nat.Syntax.Type
import Nat.Unification

data ModuleEvalError
  = MExprEvalError ExprEvalError
  | MTypeError (InferenceError Type Expr)
  deriving (Eq, Show)

type ModuleEnv = Map.Map Var Expr

type ModuleEvalT a = ExceptT ModuleEvalError Identity a

instance Reducible ModuleExpr ModuleExpr ExprEvalError ExprReductionEnv where
  reduce = mapM reduce

instance Reducible Module Module ExprEvalError ExprReductionEnv where
  reduce mod = snd <$> mapAccumM reduce' Map.empty mod
    where
      reduce' env expr = do
        expr' <- reduce (inEnv env expr)
        pure (merge expr' env, expr')
      merge modExpr modEnv = Map.union modEnv $ case modExpr of
        MDecl v e -> Map.singleton v (toExpr modExpr)
        _ -> Map.empty

toExpr = \case
  -- is the declaration recursive?
  MDecl v e | Set.member v (fv e) -> EFix v e
  MDecl _ e -> e
  MExec e -> e

toEnv t = \case
  MDecl v _ -> Map.singleton v t
  MExec _ -> Map.empty

type Assignments = Map.Map Expr Type

typeMod' :: Module -> InferenceState Type Expr -> Either (InferenceError Type Expr) (TypeEnv, Assignments)
typeMod' mod = run (foldM signifyIn (Map.empty, Map.empty) mod)
  where
    run m s = runExcept $ evalStateT (runReaderT m Map.empty) s

    signifyIn :: (TypeEnv, Assignments) -> ModuleExpr -> InferT Type Expr (TypeEnv, Assignments)
    signifyIn (env, assigns) mExpr = local (Map.union env) $ do
      (t, env', assigns') <- signify (toExpr mExpr)
      pure (Map.unions [toEnv t mExpr, env', env], Map.union assigns assigns')

typeMod :: InferenceState Type Expr -> Module -> ModuleEvalT (TypeEnv, Assignments)
typeMod s mod = case typeMod' mod s of
  Left err -> throwError $ MTypeError err
  Right env -> return env

reduceMod :: Module -> TypeEnv -> ModuleEvalT Module
reduceMod mod tyEnv = case reduceIn tyEnv mod of
  Left err -> throwError $ MExprEvalError err
  Right mod' -> pure mod'
  where
    reduceIn tyEnv = runReduce' (ExprRedEnv {tyEnv = tyEnv, relEnv = Map.empty})

collectDecls :: Module -> [Var] -> Module
collectDecls mod ids = filter incl mod
  where
    incl = \case
      MDecl v _ | elem v ids -> True
      _ -> False

dotSep :: [String] -> String
dotSep [] = ""
dotSep ws = foldr1 (\w s -> w ++ '.' : s) ws

resolveImports :: [NamedModule] -> Module -> Module
resolveImports mods mod = foldl' accum mod' imports
  where
    accum decls (MImport ids path) = case lookup path assoc of
      Nothing -> decls
      Just mod -> collectDecls mod ids <> decls
    assoc = mods <&> \(NMod p mod) -> (p, mod)
    (imports, mod') = partition (\case MImport {} -> True; _ -> False) mod

runTypeMod :: Module -> Either ModuleEvalError (TypeEnv, Assignments)
runTypeMod mod = runExcept (typeMod mkIState mod)

eval' :: [NamedModule] -> Module -> Either ModuleEvalError Module
eval' mods mod = runExcept $ (reduceMod mod' <=< (return . fst) <=< typeMod infState) mod'
  where
    rename :: Module -> (Module, Int)
    rename = runRename . fmap (fmap desugar)

    infState = IState {names = nameSupply, assignments = Map.empty}

    (mod', nameSupply) = (rename . resolveImports mods) mod

eval :: Module -> Either ModuleEvalError Module
eval = eval' []