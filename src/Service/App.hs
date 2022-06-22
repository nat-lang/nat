{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Service.App
  ( mkApp,
  )
where

import Control.Monad.Reader
import Data.Proxy
import Servant

import Service.API.Fragments
import Service.API.ConstituencyTrees
import Service.Ctx

type API =  "fragments"          :> FragmentAPI
       :<|> "constituency-trees" :> ConstituencyTreeAPI

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = fragmentHandler :<|> constituencyTreeHandler

mkApp :: Context '[] -> AppCtx -> Application
mkApp cfg ctx =
  serveWithContext api cfg $
    hoistServerWithContext api (Proxy :: Proxy '[])
      (flip runReaderT ctx) server