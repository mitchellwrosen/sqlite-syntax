-- | Backtracking text parser, with the ability to commit to a branch and prevent backtracking.
--
-- Based heavily on (read: stolen from) @headed-megaparsec@ by Nikita Volkov.
module TextParser
  ( TextParser,
    commit,
    run,
    --
    getOffset,
    --
    char,
    char_,
    char',
    eof,
    satisfy,
    space,
    string,
    string',
    takeWhile,
    takeWhile1,
  )
where

import Control.Applicative
import Control.Monad (MonadPlus (..), ap)
import Data.Coerce (coerce)
import Data.Functor
import Data.Text (Text)
import Data.Void (Void)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Megaparsec.Lexer
import Prelude hiding (takeWhile)

newtype TextParser a
  = TextParser (P (Result P a))

instance Alternative TextParser where
  empty :: forall a. TextParser a
  empty =
    coerce @(P (Result P a))
      empty

  (<|>) :: forall a. TextParser a -> TextParser a -> TextParser a
  (<|>) =
    coerce @(P (Result P a) -> P (Result P a) -> P (Result P a))
      \x y -> Megaparsec.try x <|> y

instance Applicative TextParser where
  pure :: forall a. a -> TextParser a
  pure =
    coerce @(a -> P (Result P a)) do
      pure . Done

  (<*>) :: TextParser (a -> b) -> TextParser a -> TextParser b
  (<*>) =
    ap

instance Functor TextParser where
  fmap :: forall a b. (a -> b) -> TextParser a -> TextParser b
  fmap =
    coerce @((a -> b) -> P (Result P a) -> P (Result P b))
      \f -> fmap (fmap f)

instance Monad TextParser where
  return :: a -> TextParser a
  return =
    pure

  (>>=) :: forall a b. TextParser a -> (a -> TextParser b) -> TextParser b
  (>>=) =
    coerce @(P (Result P a) -> (a -> P (Result P b)) -> P (Result P b))
      \mx f ->
        mx >>= \case
          Done x -> f x
          Commit my -> pure (Commit (my >>= f >>= runResult))

instance MonadFail TextParser where
  fail :: forall a. String -> TextParser a
  fail =
    coerce @(String -> P (Result P a)) fail

instance MonadPlus TextParser where
  mzero = empty
  mplus = (<|>)

type P =
  Megaparsec.Parsec Void Text

data Result f a
  = Done a
  | Commit (f a)

instance Functor f => Functor (Result f) where
  fmap f = \case
    Done x -> Done (f x)
    Commit x -> Commit (fmap f x)

runResult :: Applicative f => Result f a -> f a
runResult = \case
  Done x -> pure x
  Commit m -> m

commit :: TextParser ()
commit =
  coerce @(P (Result P ())) do
    pure (Commit (pure ()))

run :: forall a. TextParser a -> Text -> Either (Megaparsec.ParseErrorBundle Text Void) a
run parser input =
  Megaparsec.parse (coerce @(TextParser a) @(P (Result P a)) parser >>= runResult) "" input

-- Lifted megaparsec API

lift :: forall a. P a -> TextParser a
lift =
  coerce @(P a -> P (Result P a)) do
    fmap Done

getOffset :: TextParser Int
getOffset =
  lift Megaparsec.getOffset

char :: Char -> TextParser Char
char =
  lift . Megaparsec.char

char_ :: Char -> TextParser ()
char_ =
  void . char

char' :: Char -> TextParser Char
char' =
  lift . Megaparsec.char'

eof :: TextParser ()
eof =
  lift Megaparsec.eof

satisfy :: (Char -> Bool) -> TextParser Char
satisfy =
  lift . Megaparsec.satisfy

space :: Text -> Text -> Text -> TextParser ()
space line block0 block1 =
  lift do
    Megaparsec.Lexer.space
      Megaparsec.space1
      (Megaparsec.Lexer.skipLineComment line)
      (Megaparsec.Lexer.skipBlockComment block0 block1)

string :: Text -> TextParser Text
string =
  lift . Megaparsec.string

string' :: Text -> TextParser Text
string' =
  lift . Megaparsec.string'

takeWhile :: (Char -> Bool) -> TextParser Text
takeWhile =
  lift . Megaparsec.takeWhileP Nothing

takeWhile1 :: (Char -> Bool) -> TextParser Text
takeWhile1 =
  lift . Megaparsec.takeWhile1P Nothing
