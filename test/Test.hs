module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Text.Lazy
import Hedgehog
import qualified LexerTests
import qualified ParserTests
import qualified Sqlite.Syntax.Parser as Parser
import System.Directory
import System.Environment
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple (pPrint, pShowNoColor)
import Prelude hiding (lex, readFile)

main :: IO Bool
main = do
  -- Temporary manual parsing of args, because the full golden test suite isn't passing, so it's most convenient to run
  -- the custom little "parser" test loop
  getArgs >>= \case
    ["lexer"] -> do
      andM
        [ check (withTests 10000 (property LexerTests.propRoundTrip)),
          check (withTests 1 (property (LexerTests.unitTests)))
        ]
    ["parser"] -> do
      ParserTests.parseGoldenFiles
      pure True
    ["golden"] -> do
      sqlFiles <- listDirectory "test/files/in"
      defaultMain do
        testGroup
          "parser"
          [ localOption (SizeCutoff 4096) do
              testGroup
                "golden"
                [ testGroup "show" do
                    sqlFile <- sqlFiles
                    [ golden
                        sqlFile
                        ("test/files/in/" ++ sqlFile)
                        ("test/files/out0/" ++ replaceExtension sqlFile "hs")
                        \sql ->
                          case Parser.parseStatement sql of
                            Left err -> do
                              case err of
                                Parser.SyntaxError {} -> Text.putStrLn (Parser.renderParseError err)
                                Parser.ParseError {} -> Text.putStrLn (Parser.renderParseError err)
                                Parser.AmbiguousParse x -> pPrint x
                              exitFailure
                            Right statement -> pure (Text.Lazy.toStrict (pShowNoColor statement))
                      ]
                ]
          ]
      pure True
    _ -> do
      putStrLn "Expected argument: 'lexer', 'parser', or 'golden'"
      pure False

golden :: TestName -> FilePath -> FilePath -> (Text -> IO Text) -> TestTree
golden name infile outfile f =
  goldenVsString name outfile do
    contents <- readFile infile
    output <- f contents
    pure (ByteString.Lazy.fromStrict (Text.encodeUtf8 output))

andM :: Monad m => [m Bool] -> m Bool
andM = \case
  [] -> pure True
  m : ms ->
    m >>= \case
      False -> pure False
      True -> andM ms

readFile :: FilePath -> IO Text
readFile path = do
  bytes <- ByteString.readFile path
  pure (Text.decodeUtf8 bytes)
