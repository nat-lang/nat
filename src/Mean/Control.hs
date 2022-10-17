module Mean.Control where

import Control.Monad (MonadPlus, foldM, mplus, mzero)

foldM1 :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error "foldM1" "empty list"
foldM1 f (x : xs) = foldM f x xs

mapAccumM :: (Monad m) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumM _ z [] = return (z, [])
mapAccumM f z (x : xs) = do
  (z', y) <- f z x
  (z'', ys) <- mapAccumM f z' xs
  return (z'', y : ys)
