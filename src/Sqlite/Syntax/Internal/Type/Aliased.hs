module Sqlite.Syntax.Internal.Type.Aliased
  ( Aliased (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-- | A value aliased with a name, e.g. @table AS name@.
data Aliased f a = Aliased
  { value :: a,
    alias :: f Text
  }
  deriving stock (Generic)

deriving instance (Eq a, Eq (f Text)) => Eq (Aliased f a)

deriving instance (Show a, Show (f Text)) => Show (Aliased f a)
