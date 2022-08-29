module Mean.Common.Parser where

import Data.Text (Text)
import qualified Data.Text.IO as TiO
import qualified Text.Megaparsec as P

parse :: P.Parsec e s a -> s -> Either (P.ParseErrorBundle s e) a
parse parser = P.runParser parser "<input>"

parseFile :: P.Parsec e Text a -> String -> IO (Either (P.ParseErrorBundle Text e) a)
parseFile parser file = P.runParser parser file <$> TiO.readFile file
