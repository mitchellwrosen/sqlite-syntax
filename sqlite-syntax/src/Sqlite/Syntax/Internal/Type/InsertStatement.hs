module Sqlite.Syntax.Internal.Type.InsertStatement
  ( InsertStatement (..),
  )
where

import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Aliased (Aliased)
import Sqlite.Syntax.Internal.Type.Columns (Columns)
import Sqlite.Syntax.Internal.Type.CommonTableExpressions (CommonTableExpressions)
import Sqlite.Syntax.Internal.Type.OnConflict (OnConflict)
import Prelude

-- |
-- * https://sqlite.org/lang_insert.html
-- * https://sqlite.org/syntax/insert-stmt.html
data InsertStatement = InsertStatement
  { commonTableExpressions :: CommonTableExpressions,
    onConflict :: OnConflict,
    table :: Aliased Maybe (Columns [])
  }
  deriving stock (Eq, Generic, Show)
