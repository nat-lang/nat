module Utils where

import Data.Char (toUpper)

titleCase (c:cs) = toUpper c : cs