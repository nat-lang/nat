{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
import Data.Default (def)
import Data.Proxy
import Data.Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (cors, corsMethods, corsOrigins, corsRequestHeaders, simpleCors, simpleCorsResourcePolicy, simpleHeaders)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Prelude.Compat
import Servant
import System.Log.FastLogger
  ( LoggerSet,
    ToLogStr (..),
    defaultBufSize,
    flushLogStr,
    newStdoutLoggerSet,
    pushLogStrLn,
  )

import Service.Ctx (AppCtx(..))
import Service.App (mkApp)
import Service.Settings (SiteConfig(..))
import Service.Logger (LogMsg(..))


port :: Int
port = 8080

jsonRequestLoggerMiddleware :: IO Middleware
jsonRequestLoggerMiddleware =
  mkRequestLogger $ def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}

main :: IO ()
main = do
  -- typically, we'd create our config from environment variables
  -- but we're going to just make one here
  let config = SiteConfig "dev" "1.0.0" "admin" "secretPassword" "fragments"

  warpLoggerMiddleware <- jsonRequestLoggerMiddleware
  appLogger <- newStdoutLoggerSet defaultBufSize

  let corsPolicy =
        simpleCorsResourcePolicy
          { corsMethods = ["OPTIONS", "GET", "POST"],
            corsRequestHeaders = simpleHeaders
          }

  tstamp <- getCurrentTime

  let lgmsg =
        LogMsg
          { message = "Interpeter starting up!",
            timestamp = tstamp,
            level = "info",
            lversion = version config,
            lenvironment = environment config
          }

  pushLogStrLn appLogger (toLogStr lgmsg) >> flushLogStr appLogger

  let ctx = AppCtx config appLogger
      warpSettings = Warp.defaultSettings
      portSettings = Warp.setPort port warpSettings
      settings = Warp.setTimeout 55 portSettings
      cfg = EmptyContext

  Warp.runSettings settings $
    warpLoggerMiddleware $
      cors (const $ Just corsPolicy) $
        mkApp cfg ctx
