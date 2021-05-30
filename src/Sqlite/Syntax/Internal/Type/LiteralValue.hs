module Sqlite.Syntax.Internal.Type.LiteralValue
  ( LiteralValue (..),
  )
where

import Data.Text (Text)

data LiteralValue
  = LiteralValue'Blob Text
  | LiteralValue'CurrentDate
  | LiteralValue'CurrentTime
  | LiteralValue'CurrentTimestamp
  | LiteralValue'False
  | LiteralValue'Null
  | LiteralValue'Number Text
  | LiteralValue'String Text
  | LiteralValue'True
