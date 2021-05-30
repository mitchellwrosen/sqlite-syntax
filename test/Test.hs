module Main where

import Hedgehog
import qualified LexerTests
import Prelude hiding (lex)

main :: IO Bool
main = do
  check (withTests 10000 (property LexerTests.propRoundTrip))
