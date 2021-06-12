module Sqlite.Syntax.Internal.Type.Named
  ( Named (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data Named a = Named
  { name :: Maybe Text,
    value :: a
  }
  deriving stock (Eq, Generic, Show)
