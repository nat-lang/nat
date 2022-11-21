{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import qualified Data.Text.IO as TiO
import qualified Nat.Evaluation.Module as E
import qualified Nat.Parser as P
import qualified Nat.Syntax.Module as S
import Nat.TeX
import Options.Applicative
import System.IO (hFlush, stderr)
import Text.LaTeX.Base.Pretty

data Input
  = IFile FilePath
  | IStd String
  deriving (Show)

data Options = Options
  { oInput :: Input,
    oCheck :: Bool,
    oTypeset :: Bool
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
    <*> switch
      ( long "typeset"
          <> short 't'
          <> help "Convert the output to LaTeX."
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
        pack
          . if oTypeset options
            then prettyLaTeX . typeset
            else show
  TiO.putStrLn $ case S.runPModule input of
    Left err -> pack $ show err
    Right mod ->
      if oCheck options
        then pack $ show $ E.runTypeMod mod
        else case E.eval mod of
          Left err -> pack $ show err
          Right mod' -> out mod

  hFlush stderr
