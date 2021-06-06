module Sqlite.Syntax.Internal.Type.QualifiedTableName
  ( IndexedBy (..),
    QualifiedTableName (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Aliased
import Sqlite.Syntax.Internal.Type.SchemaQualified
import Prelude

data IndexedBy
  = IndexedBy'IndexedBy Text
  | IndexedBy'NotIndexed
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/qualified-table-name.html
data QualifiedTableName = QualifiedTableName
  { name :: Aliased Maybe (SchemaQualified Text),
    indexedBy :: Maybe IndexedBy
  }
  deriving stock (Eq, Generic, Show)
