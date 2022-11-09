{-# LANGUAGE DeriveTraversable #-}

module Nat.Syntax.Position where

data Position a = Pn
  { -- | File.
    srcFile :: !a,
    -- | Position, counting from 0.
    posPos :: !Int,
    -- | Line number, counting from 0.
    posLine :: !Int,
    -- | Column number, counting from 0.
    posCol :: !Int
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data Interval a = Interval {iStart, iEnd :: !(Position String)}
  deriving (Show, Eq, Ord)