{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (runExcept)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Text (Text, pack)
import qualified Data.Text.IO as TiO
import Debug.Trace (traceM)
import qualified Nat.Evaluation.Module as E
import qualified Nat.Parser as P
import qualified Nat.Syntax.Module as S
import Options.Applicative
import System.Environment (getArgs)

data Input
  = IFile FilePath
  | IStd String
  deriving (Show)

data Options = Options
  { oInput :: Input,
    oCheck :: Bool
  }
  deriving (Show)

input :: Parser Input
input =
  IFile
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Read from a file."
      )
    <|> IStd
      <$> argument str idm

opts :: Parser Options
opts =
  Options
    <$> input
    <*> switch
      ( long "check"
          <> short 'c'
          <> help "Typecheck without executing."
      )

entry :: ParserInfo Options
entry =
  info
    (opts <**> helper)
    ( fullDesc
        <> progDesc "Evaluate a nat grammar."
        <> header "nat v0.0.1"
    )

chooseInput :: Input -> IO Text
chooseInput = \case
  IFile f -> TiO.readFile f
  IStd i -> return (pack i)

main = do
  options <- execParser entry
  input <- chooseInput (oInput options)

  case P.parse S.pModule input of
    Left err -> print err
    Right mod ->
      print $
        if oCheck options
          then show $ E.runTypeMod mod
          else show $ E.eval mod
