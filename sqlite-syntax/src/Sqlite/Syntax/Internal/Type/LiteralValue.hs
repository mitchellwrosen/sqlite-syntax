module Sqlite.Syntax.Internal.Type.LiteralValue
  ( LiteralValue (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-- TODO rename to Literal
data LiteralValue
  = -- | /BLOB literals are string literals containing hexadecimal data and preceded by a single "x" or "X" character. Example: X'53514C697465'/
    Blob Text
  | Boolean Bool
  | -- | /CURRENT_DATE/
    CurrentDate
  | -- | /CURRENT_TIME/
    CurrentTime
  | -- | /CURRENT_TIMESTAMP/
    CurrentTimestamp
  | -- | /NULL/
    Null
  | -- | https://sqlite.org/syntax/numeric-literal.html
    Number Text
  | String Text
  deriving stock (Eq, Generic, Show)
