module Sqlite.Syntax.Internal.Type.Named
  ( Named (..),
  )
where

import Data.Text (Text)
import Prelude

data Named a = Named
  { name :: Maybe Text,
    value :: a
  }
