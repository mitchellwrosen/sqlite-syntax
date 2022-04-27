module Sqlite.Syntax.Internal.Error
  ( renderError,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import Prelude hiding (exponent, lex)

renderError :: Text -> Text -> Maybe Int -> Text
renderError message code maybeOffset =
  fromMaybe message do
    offset <- maybeOffset
    context <- maybeContext offset
    pure (message <> "\n\n" <> context)
  where
    maybeContext :: Int -> Maybe Text
    maybeContext offset = do
      let ( maybeInputLine,
            Megaparsec.PosState {pstateSourcePos = Megaparsec.SourcePos {sourceColumn, sourceLine}}
            ) =
              Megaparsec.reachOffset offset initialState
      inputLine <- maybeInputLine
      let line = Text.pack (show (Megaparsec.unPos sourceLine))
          left = Text.replicate (Text.length line) " " <> " │"
      pure do
        Text.unlines
          [ left,
            line <> " │ " <> Text.pack inputLine,
            left <> Text.replicate (Megaparsec.unPos sourceColumn) " " <> "↑"
          ]
      where
        initialState :: Megaparsec.PosState Text
        initialState =
          Megaparsec.PosState
            { pstateInput = code,
              pstateOffset = 0,
              pstateSourcePos = Megaparsec.initialPos "",
              pstateTabWidth = Megaparsec.defaultTabWidth,
              pstateLinePrefix = ""
            }
