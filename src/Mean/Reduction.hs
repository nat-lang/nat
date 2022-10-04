{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Reduction where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT)

type Reduction r s e = ReaderT s (ExceptT e Identity) r

class Reducible a b where
  reduce :: a -> Reduction s e b
