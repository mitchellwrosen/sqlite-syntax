module Sqlite.Syntax.Internal.Type.IndexedColumn
  ( IndexedColumn (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Ordering (Ordering)
import Prelude hiding (Ordering)

-- | https://sqlite.org/syntax/indexed-column.html
--
-- From https://sqlite.org/lang_createtable.html,
--
--   * /The PRIMARY KEY clause must contain only column names — the use of expressions in an indexed-column of a PRIMARY KEY is not supported./
--
--   * /As with PRIMARY KEYs, a UNIQUE table-constraint clause must contain only column names — the use of expressions in an indexed-column of a UNIQUE table-constraint is not supported./
data IndexedColumn = IndexedColumn
  { column :: Text,
    collation :: Maybe Text,
    ordering :: Ordering
  }
  deriving stock (Eq, Generic, Show)
