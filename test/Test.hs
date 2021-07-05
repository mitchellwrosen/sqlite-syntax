module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text.Lazy
import Hedgehog
import qualified LexerTests
import qualified ParserTests
import qualified Sqlite.Syntax.Parser as Parser
import System.Directory
import System.Environment
import System.FilePath (replaceExtension)
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple (pShowNoColor)
import Prelude hiding (lex, readFile)

main :: IO ()
main = do
  sqlFiles <- take 1 <$> listDirectory "test/files/in"
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
                            Parser.SyntaxError {} -> Parser.renderParseError err
                            Parser.ParseError {} -> Parser.renderParseError err
                            Parser.AmbiguousParse x -> Text.Lazy.toStrict (pShowNoColor x)
                        Right statement -> Text.Lazy.toStrict (pShowNoColor statement)
                  ]
            ]
      ]

-- getArgs >>= \case
--   ["lexer"] -> do
--     andM
--       [ check (withTests 10000 (property LexerTests.propRoundTrip)),
--         check (withTests 1 (property (LexerTests.unitTests)))
--       ]
--   ["parser"] -> do
--     ParserTests.parseGoldenFiles
--     pure True
--   _ -> do
--     putStrLn "Expected argument: 'lexer' or 'parser'"
--     pure False

golden :: TestName -> FilePath -> FilePath -> (Text -> Text) -> TestTree
golden name infile outfile f =
  goldenVsString name outfile (ByteString.Lazy.fromStrict . Text.encodeUtf8 . f <$> readFile infile)

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
