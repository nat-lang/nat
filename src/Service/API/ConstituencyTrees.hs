{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Service.API.ConstituencyTrees where
import GHC.Generics (Generic)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Aeson as JSON
import Servant (JSON, Post, ReqBody, (:>))
import qualified Compiler.Tree.Syntax as T
import qualified Compiler.Tree.Parser as P
import Service.Ctx
import Service.Serializers

data ConstituencyTreeHandlerResp = ConstituencyTreeHandlerResp
  { constituencyTree :: !T.ConstituencyTree }
  deriving (Show, Generic)

instance JSON.ToJSON ConstituencyTreeHandlerResp where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

type ConstituencyTreeAPI = ReqBody '[JSON] T.ConstituencyTree :> Post '[JSON] ConstituencyTreeHandlerResp

constituencyTreeHandler :: T.ConstituencyTree -> AppM ConstituencyTreeHandlerResp
constituencyTreeHandler ct = pure $ ConstituencyTreeHandlerResp { constituencyTree = ct }
