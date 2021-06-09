module Sqlite.Syntax.Internal.Type.OrderingTerm
  ( NullsWhich (..),
    Ordering (..),
    OrderingTerm (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression
import Prelude hiding (Ordering)

data NullsWhich
  = -- | @NULLS FIRST@
    NullsWhich'First
  | -- | @NULLS LAST@
    NullsWhich'Last
  deriving stock (Eq, Generic, Show)

data Ordering
  = -- | @ASC@
    Ordering'Asc
  | -- | @DESC@
    Ordering'Desc
  deriving stock (Eq, Generic, Show)

data OrderingTerm = OrderingTerm
  { expression :: Expression,
    collation :: Maybe Text,
    ordering :: Ordering,
    nullsWhich :: NullsWhich
  }
  deriving stock (Eq, Generic, Show)
