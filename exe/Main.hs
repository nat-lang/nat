{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import Data.Text
import qualified Data.Text.IO as TiO
import qualified Nat.Evaluation.Module as E
import qualified Nat.Parser as P
import qualified Nat.Syntax.Module as S
import Nat.TeX
import Nat.Viz (prettyPairs)
import Options.Applicative
import System.IO (hFlush, stderr)
import Text.LaTeX.Base.Pretty

data Input
  = IFile FilePath
  | IStd String
  deriving (Show)

data Options = Options
  { oInput :: Input,
    oTypecheck :: Bool,
    oTypeset :: Bool,
    oEvaluate :: Bool
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
      ( long "typecheck"
          <> short 'c'
          <> help "Typecheck without executing."
      )
    <*> switch
      ( long "typeset"
          <> short 't'
          <> help "Convert the output to LaTeX."
      )
    <*> switch
      ( long "evaluate"
          <> short 'e'
          <> help "Evaluate the file."
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
  let out =
        if oTypeset options
          then render . typeset
          else pack . show
  TiO.putStrLn $ case S.runPModule input of
    Left err -> pack $ show err
    Right mod ->
      -- todo: make a matrix of the options
      if oTypecheck options
        then case E.runTypeMod mod of
          Left err -> pack $ show err
          Right (_, assignments) -> (pack . show . prettyPairs "->" . Map.toList) assignments
        else
          if oEvaluate options
            then case E.eval mod of
              Left err -> pack $ show err
              Right mod' -> out mod'
            else out mod

  hFlush stderr
