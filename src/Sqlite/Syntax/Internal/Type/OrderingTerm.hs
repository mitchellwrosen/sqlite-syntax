module Sqlite.Syntax.Internal.Type.OrderingTerm where

import Data.Text (Text)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression
import Prelude hiding (Ordering)

data NullsWhich
  = -- | @NULLS FIRST@
    NullsWhich'First
  | -- | @NULLS LAST@
    NullsWhich'Last

data Ordering
  = -- | @ASC@
    Ordering'Asc
  | -- | @DESC@
    Ordering'Desc

data OrderingTerm = OrderingTerm
  { expression :: Expression,
    collation :: Maybe Text,
    ordering :: Ordering,
    nullsWhich :: Maybe NullsWhich
  }
