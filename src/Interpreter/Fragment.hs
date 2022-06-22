module Interpreter.Fragment (Fragment, loadFragment, mkFragment) where

import System.IO
import qualified Data.Either as E
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Compiler.Core.Inference as Inf
import qualified Compiler.Core.TypeEnv as TyEnv
import qualified Compiler.Core.Syntax as S
import qualified Compiler.Core.Parser as Parse

type Fragment = Map.Map String S.Expr

{-
typeCheckAccumulator :: TyEnv.Env -> S.Decl -> (TyEnv.Env, CheckResult)
typeCheckAccumulator ctx decl = case decl of
  -- tech debt -- in the future typedefs will not be limited to consts
  S.Typedef name ty -> (ctx ++ [(name,ty)], Right (name, (S.Const name ty, ty)))
  S.Let name expr -> case Inf.inferExpr ctx expr of
    Right ty -> (ctx ++ [(name,ty)], Right (name, (expr, ty)))
    Left err -> (ctx, Left err)

typeCheck :: [S.Decl] -> [E.Either Inf.TypeError (String, LexicalEntry)]
typeCheck = snd . (L.mapAccumL typeCheckAccumulator [])

loadDecls :: [S.Decl] -> E.Either LoadError Fragment
loadDecls decls = do
  let (errs, tyCheckedDecls) = E.partitionEithers $ typeCheck decls
  if null errs
    then Right (Map.fromList tyCheckedDecls)
    else Left (LTyError errs)
-}

mkFragment :: [S.Decl] -> Fragment
mkFragment = Map.fromList . (map toTup)
  where
    toTup d = case d of
      S.Let n e -> (n,e)
      -- fixme: in the future typedefs will not be limited to consts
      S.Typedef n t -> (n, S.Const n t)

loadFragment :: FilePath -> IO (E.Either Parse.ParseError Fragment)
loadFragment fp = do
  fragIO <- Parse.parseFrag fp
  case fragIO of
    Left parErr -> pure $ Left parErr
    Right decls -> pure $ Right $ mkFragment decls