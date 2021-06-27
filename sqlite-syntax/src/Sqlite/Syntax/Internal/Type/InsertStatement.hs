module Sqlite.Syntax.Internal.Type.InsertStatement
  ( Insert (..),
    InsertStatement (..),
  )
where

import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Aliased (Aliased)
import Sqlite.Syntax.Internal.Type.Columns (Columns)
import Sqlite.Syntax.Internal.Type.CommonTableExpressions (CommonTableExpressions)
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.IndexedColumn (IndexedColumn)
import Sqlite.Syntax.Internal.Type.OnConflict (ConflictResolution)
import Sqlite.Syntax.Internal.Type.Returning (Returning)
import Sqlite.Syntax.Internal.Type.SelectStatement (SelectStatement)
import Sqlite.Syntax.Internal.Type.Values (Values)
import Prelude

-- | https://sqlite.org/lang_upsert.html
data ConflictTarget = ConflictTarget
  { columns :: NonEmpty IndexedColumn,
    where_ :: Maybe Expression
  }
  deriving stock (Eq, Generic, Show)

data Insert
  = InsertDefaultValues
  | InsertSelect SelectStatement (Maybe UpsertClauses)
  | InsertValues Values (Maybe UpsertClauses)
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/lang_insert.html
data InsertStatement = InsertStatement
  { commonTableExpressions :: CommonTableExpressions,
    onConflict :: ConflictResolution,
    table :: Columns (Aliased Maybe) [],
    insert :: Insert,
    returning :: Maybe Returning
  }
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/lang_upsert.html
data UpsertAction
  = DoNothing
  | DoUpdate (NonEmpty (NonEmpty Text, Expression)) (Maybe Expression)
  deriving stock (Eq, Generic, Show)

-- | https://www.sqlite.org/syntax/upsert-clause.html
data UpsertClause f
  = UpsertClause (f ConflictTarget) UpsertAction
  deriving stock (Generic)

deriving instance Eq (f ConflictTarget) => Eq (UpsertClause f)

deriving instance Show (f ConflictTarget) => Show (UpsertClause f)

-- | https://sqlite.org/lang_upsert.html
data UpsertClauses
  = UpsertClauseNil (UpsertClause Maybe)
  | UpsertClauseCons (UpsertClause Identity) UpsertClauses
  deriving stock (Eq, Generic, Show)
