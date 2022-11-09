{
module Nat.Syntax.Lexer (Token(..),P,evalP,lexer) where
import Nat.Syntax.Tokens
import Control.Monad.State
import Control.Monad.Error
import Data.Word
}

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

{

data AlexState = AlexState
  { alex_pos   :: !AlexPosn  -- position at current input location
  , alex_inp   :: String     -- the current input
  , alex_chr   :: !Char      -- the character before the input
  , alex_bytes :: [Byte]     -- rest of the bytes for the current char
  , alex_scd   :: !Int       -- the current startcode
  }

newtype Alex a = Alex { unAlex :: AlexState
                               -> Either String (AlexState, a) }

instance Functor     Alex where ...
instance Applicative Alex where ...
instance Monad       Alex where ...

runAlex          :: String -> Alex a -> Either String a

type AlexInput =
  ( AlexPosn                 -- current position,
  , Char                     -- previous char
  , [Byte]                   -- rest of the bytes for the current char
  , String                   -- current input string
  )

alexGetInput     :: Alex AlexInput
alexSetInput     :: AlexInput -> Alex ()

alexError        :: String -> Alex a

alexGetStartCode :: Alex Int
alexSetStartCode :: Int -> Alex ()
}