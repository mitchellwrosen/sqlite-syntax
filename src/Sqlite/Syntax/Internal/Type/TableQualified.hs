module Sqlite.Syntax.Internal.Type.TableQualified
  ( TableQualified (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data TableQualified a
  = TableQualified (Maybe Text) a
  deriving stock (Eq, Generic, Show)
