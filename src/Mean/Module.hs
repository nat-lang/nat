module Mean.Module (Module, loadModule, mkModule) where

import System.IO
import qualified Data.Either as E
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Mean.TypeEnv as TyEnv
import qualified Mean.Syntax as S
import qualified Mean.Parser as P

type Module = Map.Map S.Name S.Expr

mkModule :: [S.Expr] -> Module
mkModule = Map.fromList . map toTup
  where
    toTup d = case d of
      S.Let n e -> (n,e)

-- loadModule :: FilePath -> IO (E.Either P.ParseError Module)
loadModule fp = do
  fragIO <- P.pFModule fp
  case fragIO of
    Left parErr -> pure $ Left parErr
    Right decls -> pure $ Right $ mkModule decls