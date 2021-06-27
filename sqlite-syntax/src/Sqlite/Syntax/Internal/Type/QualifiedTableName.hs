module Sqlite.Syntax.Internal.Type.QualifiedTableName
  ( IndexedBy (..),
    QualifiedTableName (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Aliased
import Sqlite.Syntax.Internal.Type.Namespaced
import Prelude

data IndexedBy
  = IndexedBy Text
  | NotIndexed
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/qualified-table-name.html
data QualifiedTableName = QualifiedTableName
  { name :: Aliased Maybe (Namespaced Text Text),
    indexedBy :: Maybe IndexedBy
  }
  deriving stock (Eq, Generic, Show)
