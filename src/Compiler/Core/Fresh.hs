module Compiler.Core.Fresh
  ( letters
  )
where

import Control.Monad (replicateM)

letters :: [String]
letters = [1 ..] >>= flip replicateM ['A' .. 'Z']
