module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Text.IO as Text
import qualified Sqlite.Syntax.Parser as Parser
import System.INotify
import Text.Pretty.Simple (pPrint)
import Prelude

main :: IO ()
main =
  withINotify \i -> do
    _ <- addWatch i [AllEvents] "." \case
      Modified False (Just "debug.sql") -> do
        sql <- Text.readFile "debug.sql"
        case Parser.parseStatement sql of
          Left err ->
            case err of
              Parser.SyntaxError {} -> Text.putStrLn (Parser.renderParseError err)
              Parser.ParseError {} -> Text.putStrLn (Parser.renderParseError err)
              Parser.AmbiguousParse x -> pPrint x
          Right statement -> pPrint statement
      _ -> pure ()
    putStrLn "watching `debug.sql` for changes"
    forever (threadDelay maxBound)
