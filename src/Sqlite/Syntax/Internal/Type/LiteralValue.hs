module Sqlite.Syntax.Internal.Type.LiteralValue
  ( LiteralValue (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-- TODO rename to Literal
data LiteralValue
  = Blob Text
  | Boolean Bool
  | -- | /CURRENT_DATE/
    CurrentDate
  | -- | /CURRENT_TIME/
    CurrentTime
  | -- | /CURRENT_TIMESTAMP/
    CurrentTimestamp
  | -- | /NULL/
    Null
  | Number Text
  | String Text
  deriving stock (Eq, Generic, Show)
