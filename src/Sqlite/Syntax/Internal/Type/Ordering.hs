module Sqlite.Syntax.Internal.Type.Ordering
  ( Ordering (..),
  )
where

import GHC.Generics (Generic)
import Prelude hiding (Ordering)

data Ordering
  = -- | /ASC/
    Ascending
  | -- | /DESC/
    Descending
  deriving stock (Eq, Generic, Show)
