module Sqlite.Syntax.Internal.Type.Namespaced
  ( Namespaced (..),
  )
where

import GHC.Generics (Generic)
import Prelude

data Namespaced a b = Namespaced
  { namespace :: Maybe a,
    value :: b
  }
  deriving stock (Eq, Generic, Show)
