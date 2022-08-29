module Mean.Sugar.Type where

import Mean.Core.TypeEnv
import Mean.Core.Type as Core
import Mean.Sugar.Syntax

-- | Type an expression tree, passing down binder assignments.
infer :: ExprTree -> (TypeCheckedExprTree, TyEnv)
infer eTree = runState (mapM typeCheck eTree) empty
  where
    updateTypeEnv expr gTy = \env -> case expr of
      (S.EBinder (S.Binder n t)) -> TyEnv.extend env (n, gTy)
      _ -> env

    typeCheck :: ExprLabel -> State TyEnv.Env TypeCheckedExprLabel
    typeCheck (eLabel, cLabel) = case eLabel of
      Nothing -> pure (Nothing, cLabel)
      Just expr -> do
        env <- get
        case Inf.inferExpr env expr of
          Left err -> pure (Just $ UntypedExpr expr err, cLabel)
          Right ty -> case ty of
            gTy@(S.Forall _ iTy) -> do
              modify (updateTypeEnv expr gTy)
              pure $ (Just $ TypedExpr expr iTy, cLabel)