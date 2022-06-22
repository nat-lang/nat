{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Logger
  ( LogMsg(..),
    logMsg
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import System.Log.FastLogger
  ( LoggerSet,
    ToLogStr,
    flushLogStr,
    pushLogStrLn,
    toLogStr,
  )

import Service.Ctx
import Service.Settings

data LogMsg = LogMsg
  { message :: !Text,
    timestamp :: !UTCTime,
    level :: !Text,
    lversion :: !Text,
    lenvironment :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON LogMsg

instance ToJSON LogMsg where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMsg where
  toLogStr = toLogStr . encode

logMsg :: Text -> AppM ()
logMsg msg = do
  logset <- asks _getLogger
  tstamp <- liftIO getCurrentTime
  config <- asks _getConfig

  liftIO $ pushLogStrLn logset $ toLogStr LogMsg
    { message = msg,
      timestamp = tstamp,
      level = "info",
      lversion = version config,
      lenvironment = environment config
    }