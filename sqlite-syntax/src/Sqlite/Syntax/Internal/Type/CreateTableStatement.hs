module Sqlite.Syntax.Internal.Type.CreateTableStatement
  ( CreateTableStatement (..),
    TableConstraint (..),
    TableDefinition (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.ColumnDefinition (ColumnDefinition)
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.ForeignKeyClause (ForeignKeyClause)
import Sqlite.Syntax.Internal.Type.IndexedColumn (IndexedColumn)
import Sqlite.Syntax.Internal.Type.Named (Named)
import Sqlite.Syntax.Internal.Type.Namespaced (Namespaced)
import Sqlite.Syntax.Internal.Type.OnConflict (ConflictResolution)
import Sqlite.Syntax.Internal.Type.SelectStatement (SelectStatement)
import Prelude

-- | https://sqlite.org/syntax/create-table-stmt.html
data CreateTableStatement = CreateTableStatement
  { temporary :: Bool,
    ifNotExists :: Bool,
    name :: Namespaced Text Text,
    definition :: Either SelectStatement TableDefinition
  }
  deriving stock (Eq, Generic, Show)

data TableConstraint
  = TableConstraint'Check Expression
  | TableConstraint'ForeignKey (NonEmpty Text) ForeignKeyClause
  | TableConstraint'PrimaryKey (NonEmpty IndexedColumn) ConflictResolution
  | TableConstraint'Unique (NonEmpty IndexedColumn) ConflictResolution
  deriving stock (Eq, Generic, Show)

data TableDefinition = TableDefinition
  { columns :: NonEmpty ColumnDefinition,
    constraints :: [Named TableConstraint],
    withoutRowid :: Bool
  }
  deriving stock (Eq, Generic, Show)
