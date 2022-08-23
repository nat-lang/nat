module REPL (main) where

import Compiler.Core.Inference
import Compiler.Core.Parser
import Compiler.Core.Pretty
import Compiler.Core.Syntax
import qualified Compiler.Core.TypeEnv as TyEnv
import Control.Monad.Trans
import Interpreter.Evaluation
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let expr = parseExpr line
  case expr of
    Left err -> print err
    Right ex -> do
      print ex
      let chk = inferExpr TyEnv.empty ex
      case chk of
        Left tyerr -> print tyerr
        Right ty -> do
          print $ show ex
          print $ runEval ex

processDecl :: String -> IO ()
processDecl line = do
  let decl = parseDecl line
  case decl of
    Left err -> print err
    Right d@(Let name ex) -> do
      print d
      let chk = inferExpr TyEnv.empty ex
      case chk of
        Left tyerr -> print tyerr
        Right ty -> do
          print $ show ex
          print $ runEval ex

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      mInput <- getInputLine "λ> "
      case mInput of
        Nothing -> outputStrLn "Goodbye."
        Just input
          | length input > 0 -> (liftIO $ processDecl input) >> loop
          | otherwise -> loop