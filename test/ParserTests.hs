module ParserTests where

import qualified Data.ByteString as ByteString
import Data.Foldable
import Data.Function
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Sqlite.Syntax.Parser as Parser
import System.Directory
import Text.Pretty.Simple (pPrint)
import Prelude hiding (readFile)

parseGoldenFiles :: IO ()
parseGoldenFiles = do
  files0 <- listDirectory "test/files"
  (`fix` files0) \loop -> \case
    [] -> pure ()
    file : files -> do
      putStrLn file
      putStrLn (replicate (length file) '-')
      sql <- readFile ("test/files/" ++ file)
      Text.putStrLn sql
      Text.putStrLn "\n==>\n"
      case Parser.parseStatement sql of
        Left err -> do
          case err of
            Parser.SyntaxError {} -> Text.putStrLn (Parser.renderParseError err)
            Parser.ParseError {} -> Text.putStrLn (Parser.renderParseError err)
            Parser.AmbiguousParse x -> pPrint x
        Right statement -> do
          Text.putStrLn (Text.pack (show statement))
          _ <- getLine
          loop files

readFile :: FilePath -> IO Text
readFile path = do
  bytes <- ByteString.readFile path
  pure (Text.decodeUtf8 bytes)
