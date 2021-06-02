module Main where

import Hedgehog
import qualified LexerTests
import qualified ParserTests
import System.Environment
import Prelude hiding (lex)

main :: IO Bool
main = do
  getArgs >>= \case
    ["lexer"] -> check (withTests 10000 (property LexerTests.propRoundTrip))
    ["parser"] -> do
      ParserTests.parseGoldenFiles
      pure True
    _ -> do
      putStrLn "Expected argument: 'lexer' or 'parser'"
      pure False
