module Sqlite.Syntax.Internal.Type.Aliased
  ( Aliased (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data Aliased a = Aliased
  { value :: a,
    alias :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
