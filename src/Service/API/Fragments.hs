{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Service.API.Fragments where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import System.FilePath ((</>), (<.>))
import GHC.Generics (Generic)
import Data.Proxy
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Aeson.Text as JSONText
import qualified Data.Aeson as JSON
import qualified Data.Text as T

import Servant (Capture, JSON, Post, ReqBody, err400, throwError, (:>))

import qualified Compiler.Tree.Syntax as ST
import qualified Interpreter.Fragment as F
import qualified Interpreter.Compose as C

import Service.Ctx
import Service.Settings
import Service.Logger (logMsg)
import Service.Serializers

data FragmentHandlerResp = FragmentHandlerResp
  { semanticTree :: !C.SemanticTree }
  deriving (Show, Generic)

instance JSON.ToJSON FragmentHandlerResp where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

type FragmentAPI = Capture "fragmentId" String :> ReqBody '[JSON] ST.ConstituencyTree :> Post '[JSON] FragmentHandlerResp

encodeTreeToText :: ST.ConstituencyTree -> Text
encodeTreeToText = toStrict . toLazyText . JSONText.encodeToTextBuilder . JSON.toJSON

fragmentHandler :: String -> ST.ConstituencyTree -> AppM FragmentHandlerResp
fragmentHandler fragmentId syntaxTree = do
  config <- asks _getConfig

  logMsg $ "fragment: " <> T.pack fragmentId <> " syntax tree: " <> encodeTreeToText syntaxTree

  fragIO <- liftIO $ F.loadFragment $ (fragmentDir config) </> fragmentId <.> "hs"

  case fragIO of
    Left err -> throwError err400
    Right frag -> pure $ FragmentHandlerResp { semanticTree = C.compose frag syntaxTree }