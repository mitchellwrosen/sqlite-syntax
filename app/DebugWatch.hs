module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Text.IO as Text
import qualified Sqlite.Syntax.Parser as Parser
import qualified Sqlite.Syntax.Lexer as Lexer
import System.INotify
import Text.Pretty.Simple (pPrint)
import Prelude

main :: IO ()
main =
  withINotify \i -> do
    _ <- addWatch i [AllEvents] "." \case
      Modified False (Just "debug.sql") -> do
        sql <- Text.readFile "debug.sql"

        case Lexer.lex sql of
          Left _err -> pure ()
          Right tokens -> pPrint tokens

        case Parser.parseStatement sql of
          Left err ->
            case err of
              Parser.LexerError {} -> Text.putStrLn (Parser.renderParserError err)
              Parser.ParserError {} -> Text.putStrLn (Parser.renderParserError err)
              Parser.AmbiguousParse x -> pPrint x
          Right statement -> pPrint statement
      _ -> pure ()
    putStrLn "watching `debug.sql` for changes"
    forever (threadDelay maxBound)
