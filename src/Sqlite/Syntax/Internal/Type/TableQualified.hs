module Sqlite.Syntax.Internal.Type.TableQualified
  ( TableQualified (..),
  )
where

import Data.Text (Text)
import Prelude

data TableQualified a
  = TableQualified (Maybe Text) a
