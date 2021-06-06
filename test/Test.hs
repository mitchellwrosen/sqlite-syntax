module Main where

import Hedgehog
import qualified LexerTests
import qualified ParserTests
import System.Environment
import Prelude hiding (lex)

main :: IO Bool
main = do
  getArgs >>= \case
    ["lexer"] -> do
      andM
        [ check (withTests 10000 (property LexerTests.propRoundTrip)),
          check (withTests 1 (property (LexerTests.unitTests)))
        ]
    ["parser"] -> do
      ParserTests.parseGoldenFiles
      pure True
    _ -> do
      putStrLn "Expected argument: 'lexer' or 'parser'"
      pure False

andM :: Monad m => [m Bool] -> m Bool
andM = \case
  [] -> pure True
  m : ms ->
    m >>= \case
      False -> pure False
      True -> andM ms
