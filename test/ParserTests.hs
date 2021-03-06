module ParserTests where

import qualified Data.ByteString as ByteString
import Data.Foldable
import Data.Function
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Prettyprinter (pretty)
import qualified Sqlite.Syntax.Parser as Parser
import Sqlite.Syntax.Pretty ()
import System.Directory
import Text.Pretty.Simple (pPrint)
import Prelude hiding (readFile)

parseGoldenFiles :: IO ()
parseGoldenFiles = do
  files0 <- listDirectory "test/files/in"
  (`fix` files0) \loop -> \case
    [] -> pure ()
    file : files -> do
      putStrLn file
      putStrLn (replicate (length file) '-')
      sql <- readFile ("test/files/in/" ++ file)
      Text.putStrLn sql
      Text.putStrLn "\n==>\n"
      case Parser.parseStatements sql of
        Left err -> do
          case err of
            Parser.LexerError {} -> Text.putStrLn (Parser.renderParserError err)
            Parser.ParserError {} -> Text.putStrLn (Parser.renderParserError err)
            Parser.AmbiguousParse x -> pPrint x
        Right statements -> do
          for_ statements (print . pretty)
          -- putStrLn ""
          -- pPrint statement
          _ <- getLine
          loop files

readFile :: FilePath -> IO Text
readFile path = do
  bytes <- ByteString.readFile path
  pure (Text.decodeUtf8 bytes)
