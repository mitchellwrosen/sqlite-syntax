module Sqlite.Syntax.Internal.Type.DeleteStatement
  ( DeleteStatement (..),
  )
where

import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.CommonTableExpressions (CommonTableExpressions)
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.QualifiedTableName
import Sqlite.Syntax.Internal.Type.Returning (Returning)
import Prelude

-- | https://sqlite.org/lang_delete.html
--
-- @
-- [__common-table-expressions__]
--   /DELETE FROM/ __qualified-table-name__
--   [/WHERE/ __expression__]
--   [__returning__]
-- @
data DeleteStatement = DeleteStatement
  { commonTableExpressions :: Maybe CommonTableExpressions,
    table :: QualifiedTableName,
    where_ :: Maybe Expression,
    returning :: Maybe Returning
  }
  deriving stock (Eq, Generic, Show)
