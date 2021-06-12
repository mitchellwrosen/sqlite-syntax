module Sqlite.Syntax.Internal.Type.AttachStatement
  ( AttachStatement (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Prelude

-- | https://sqlite.org/syntax/attach-stmt.html
data AttachStatement = AttachStatement
  { database :: Expression,
    schema :: Text
  }
  deriving stock (Eq, Generic, Show)
