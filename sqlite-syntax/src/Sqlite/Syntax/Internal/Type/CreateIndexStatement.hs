module Sqlite.Syntax.Internal.Type.CreateIndexStatement
  ( CreateIndexStatement (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.IndexedColumn (IndexedColumn)
import Sqlite.Syntax.Internal.Type.Namespaced (Namespaced)
import Prelude

-- | https://sqlite.org/syntax/create-index-stmt.html
data CreateIndexStatement = CreateIndexStatement
  { unique :: Bool,
    ifNotExists :: Bool,
    name :: Namespaced Text Text,
    table :: Text,
    columns :: NonEmpty IndexedColumn,
    where_ :: Maybe Expression
  }
  deriving stock (Eq, Generic, Show)
