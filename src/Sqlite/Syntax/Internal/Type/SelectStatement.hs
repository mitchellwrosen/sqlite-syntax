module Sqlite.Syntax.Internal.Type.SelectStatement where

import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Aliased (Aliased)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.FunctionCall
import Sqlite.Syntax.Internal.Type.OrderingTerm
import Sqlite.Syntax.Internal.Type.QualifiedTableName
import Sqlite.Syntax.Internal.Type.SchemaQualified
import Sqlite.Syntax.Internal.Type.TableQualified
import Sqlite.Syntax.Internal.Type.Window
import Prelude hiding (Ordering, fail, not, null)

-- | https://sqlite.org/syntax/common-table-expression.html
data CommonTableExpression = CommonTableExpression
  { table :: Text,
    columns :: Maybe (NonEmpty Text),
    -- | @AS [NOT] MATERIALIZED@
    materialized :: Maybe Bool,
    select :: SelectStatement
  }
  deriving stock (Eq, Generic, Show)

data CompoundSelect
  = CompoundSelect SelectCore
  | -- | @... EXCEPT ...@
    CompoundSelect'Except CompoundSelect SelectCore
  | -- | @... INTERSECT ...@
    CompoundSelect'Intersect CompoundSelect SelectCore
  | -- | @... UNION ...@
    CompoundSelect'Union CompoundSelect SelectCore
  | -- | @... UNION ALL ...@
    CompoundSelect'UnionAll CompoundSelect SelectCore
  deriving stock (Eq, Generic, Show)

data GroupByClause = GroupByClause
  { -- | @GROUP BY ...@
    groupBy :: NonEmpty Expression,
    -- | @HAVING ...@
    having :: Maybe Expression
  }
  deriving stock (Eq, Generic, Show)

-- | https://www.sqlite.org/syntax/join-constraint.html
data JoinConstraint
  = -- | @ON ...@
    JoinConstraint'On Expression
  | -- | @USING ...@
    JoinConstraint'Using (NonEmpty Text)
  deriving stock (Eq, Generic, Show)

data LimitClause = LimitClause
  { -- | @LIMIT ...@
    limit :: Expression,
    -- | @OFFSET ...@
    offset :: Maybe Expression
  }
  deriving stock (Eq, Generic, Show)

data Select = Select
  { -- | @DISTINCT@
    distinct :: Bool,
    columns :: NonEmpty (SchemaQualified (TableQualified Text)),
    -- | @FROM ...@
    from :: Maybe Table,
    -- | @WHERE ...@
    where_ :: Maybe Expression,
    -- | @GROUP BY ...@
    groupBy :: Maybe GroupByClause,
    window :: Maybe (NonEmpty (Aliased Identity Window))
  }
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/select-core.html
data SelectCore
  = -- | @SELECT ...@
    SelectCore'Select Select
  | -- | @VALUES ...@
    SelectCore'Values (NonEmpty (NonEmpty Expression))
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/select-stmt.html
data SelectStatement = SelectStatement
  { -- | @WITH ...@
    with :: Maybe WithClause,
    -- | @SELECT ...@
    select :: CompoundSelect,
    -- | @ORDER BY ...@
    orderBy :: Maybe (NonEmpty OrderingTerm),
    -- | @LIMIT ...@
    limit :: Maybe LimitClause
  }
  deriving stock (Eq, Generic, Show)

-- |
-- * https://www.sqlite.org/syntax/join-clause.html
-- * https://www.sqlite.org/syntax/table-or-subquery.html
data Table
  = Table QualifiedTableName
  | Table'CrossJoin Table Table (Maybe JoinConstraint)
  | Table'Function (Aliased Maybe (FunctionCall NonEmpty))
  | Table'InnerJoin Table Table (Maybe JoinConstraint)
  | Table'LeftOuterJoin Table Table (Maybe JoinConstraint)
  | Table'NaturalCrossJoin Table Table
  | Table'NaturalInnerJoin Table Table
  | Table'NaturalLeftOuterJoin Table Table
  | Table'Subquery (Aliased Maybe SelectStatement)
  deriving stock (Eq, Generic, Show)

data WithClause = WithClause
  { -- | @RECURSIVE@
    recursive :: Bool,
    commonTableExpressions :: NonEmpty CommonTableExpression
  }
  deriving stock (Eq, Generic, Show)
