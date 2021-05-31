module Sqlite.Syntax.Internal.Type.Aliased
  ( Aliased (..),
  )
where

import Data.Text (Text)
import Prelude

data Aliased a = Aliased
  { value :: a,
    alias :: Maybe Text
  }
