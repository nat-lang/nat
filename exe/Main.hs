{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Text (Text, pack)
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

main = do
  options <- execParser entry
  case oInput options of
    IFile f -> do
      eM <- S.pFModule f
      case runIdentity eM of
        Left err -> print err
        Right m -> case E.eval m of
          Right m' -> print m'
    IStd i -> do
      case P.parse S.pModule (pack i) of
        Left err -> print err
        Right m -> case E.eval m of
          Right m' -> print m'
