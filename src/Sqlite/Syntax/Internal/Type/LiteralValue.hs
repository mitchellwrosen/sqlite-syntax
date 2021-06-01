module Sqlite.Syntax.Internal.Type.LiteralValue
  ( LiteralValue (..),
  )
where

import Data.Text (Text)
import Prelude

data LiteralValue
  = LiteralValue'Blob Text
  | LiteralValue'Boolean Bool
  | -- | @CURRENT_DATE@
    LiteralValue'CurrentDate
  | -- | @CURRENT_TIME@
    LiteralValue'CurrentTime
  | -- | @CURRENT_TIMESTAMP@
    LiteralValue'CurrentTimestamp
  | -- | @NULL@
    LiteralValue'Null
  | LiteralValue'Number Text
  | LiteralValue'String Text
