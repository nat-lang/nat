{
module Nat.Syntax.Lexer (Token(..),P,evalP,lexer) where
import Control.Monad.State
import Control.Monad.Error
import Data.Word
}

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
