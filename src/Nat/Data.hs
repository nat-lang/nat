module Nat.Data where

import Data.Char

incr :: Char -> Char
incr c = case ord c of
  122 -> 'a' -- z
  90 -> 'A' -- Z
  c' -> chr (c' + 1)