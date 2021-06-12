module Sqlite.Syntax.Internal.Type.OrderingTerm
  ( NullsPlacement (..),
    OrderingTerm (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.Ordering (Ordering)
import Prelude hiding (Ordering)

data NullsPlacement
  = -- | /NULLS FIRST/
    NullsFirst
  | -- | /NULLS LAST/
    NullsLast
  deriving stock (Eq, Generic, Show)

-- | https://www.sqlite.org/syntax/ordering-term.html
--
-- @
-- ∙ COLLATE ∙ ASC\/DESC NULLS FIRST\/LAST
-- @
data OrderingTerm = OrderingTerm
  { expression :: Expression,
    collation :: Maybe Text,
    ordering :: Ordering,
    nullsPlacement :: NullsPlacement
  }
  deriving stock (Eq, Generic, Show)
