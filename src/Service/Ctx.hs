module Service.Ctx
  ( AppCtx (..),
    AppM
  )
where

import Control.Monad.Reader
import Servant (Handler)
import Service.Settings (SiteConfig (..))
import System.Log.FastLogger (LoggerSet)

data AppCtx = AppCtx
  { _getConfig :: SiteConfig,
    _getLogger :: LoggerSet
  }

type AppM = ReaderT AppCtx Handler