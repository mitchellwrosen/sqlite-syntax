module ParserTests where

import qualified Data.ByteString as ByteString
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Sqlite.Syntax.Parser as Parser
import System.Directory
import Prelude hiding (readFile)

parseGoldenFiles :: IO ()
parseGoldenFiles = do
  files <- listDirectory "test/files"
  for_ files \file -> do
    putStrLn file
    sql <- readFile ("test/files/" ++ file)
    case Parser.parseStatement sql of
      Left err -> do
        Text.putStrLn sql
        Text.putStrLn "\n==>\n"
        Text.putStrLn err
      Right statement -> Text.putStrLn (Text.pack (show statement))
    _ <- getLine
    pure ()

readFile :: FilePath -> IO Text
readFile path = do
  bytes <- ByteString.readFile path
  pure (Text.decodeUtf8 bytes)
