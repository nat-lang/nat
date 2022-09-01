module Mean.Common.Parser where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State
  ( evalStateT,
  )
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text.IO as TiO
import Mean.Common.Lexer
import qualified Text.Megaparsec as P

parse parser i = runIdentity $ evalStateT (P.runParserT parser "<input>" i) (ParseState {inTree = False})

parseFile parser file = TiO.readFile file <&> \i -> evalStateT (P.runParserT parser file i) (ParseState {inTree = False})
