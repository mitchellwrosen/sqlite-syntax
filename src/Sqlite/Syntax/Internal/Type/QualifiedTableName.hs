module Sqlite.Syntax.Internal.Type.QualifiedTableName
  ( IndexedBy (..),
    QualifiedTableName (..),
  )
where

import Data.Text (Text)
import Sqlite.Syntax.Internal.Type.Aliased
import Sqlite.Syntax.Internal.Type.SchemaQualified
import Prelude

data IndexedBy
  = IndexedBy'IndexedBy Text
  | IndexedBy'NotIndexed

-- | https://sqlite.org/syntax/qualified-table-name.html
data QualifiedTableName
  = QualifiedTableName (Aliased (SchemaQualified Text)) (Maybe IndexedBy)
