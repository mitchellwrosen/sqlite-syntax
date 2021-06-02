module Sqlite.Syntax.Internal.Type.SchemaQualified
  ( SchemaQualified (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data SchemaQualified a
  = SchemaQualified (Maybe Text) a
  deriving stock (Eq, Generic, Show)
