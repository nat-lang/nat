{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Service.API.Fragments where

import qualified Compiler.Tree.Syntax as ST
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSONText
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import GHC.Generics (Generic)
import qualified Interpreter.Composition as C
import qualified Interpreter.Fragment as F
import Servant (Capture, JSON, Post, ReqBody, err400, throwError, (:>))
import Service.Ctx
import Service.Logger (logMsg)
import Service.Serializers
import Service.Settings
import System.FilePath ((<.>), (</>))

data FragmentHandlerResp = FragmentHandlerResp
  {semanticTree :: !C.SemanticTree}
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

  fragIO <- liftIO $ F.loadFragment $ (fragmentDir config) </> fragmentId <.> "g"

  case fragIO of
    Left err -> throwError err400
    Right frag -> pure $ FragmentHandlerResp {semanticTree = C.compose frag syntaxTree}