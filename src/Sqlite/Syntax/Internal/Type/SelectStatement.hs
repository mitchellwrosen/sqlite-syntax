module Sqlite.Syntax.Internal.Type.SelectStatement where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Sqlite.Syntax.Internal.Type.Aliased
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.FunctionCall
import Sqlite.Syntax.Internal.Type.OrderingTerm
import Sqlite.Syntax.Internal.Type.QualifiedTableName
import Sqlite.Syntax.Internal.Type.SchemaQualified
import Sqlite.Syntax.Internal.Type.TableQualified
import Prelude hiding (Ordering, fail, not, null)

-- | https://sqlite.org/syntax/common-table-expression.html
data CommonTableExpression = CommonTableExpression
  { table :: Text,
    columns :: Maybe (NonEmpty Text),
    -- | @AS [NOT] MATERIALIZED@
    materialized :: Maybe Bool,
    select :: SelectStatement
  }

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

data GroupByClause = GroupByClause
  { -- | @GROUP BY ...@
    groupBy :: NonEmpty Expression,
    -- | @HAVING ...@
    having :: Maybe Expression
  }

-- | https://www.sqlite.org/syntax/join-constraint.html
data JoinConstraint
  = -- | @ON ...@
    JoinConstraint'On Expression
  | -- | @USING ...@
    JoinConstraint'Using (NonEmpty Text)

data LimitClause = LimitClause
  { -- | @LIMIT ...@
    limit :: Expression,
    -- | @OFFSET ...@
    offset :: Maybe Expression
  }

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
    window :: Maybe WindowClause
  }

-- | https://sqlite.org/syntax/select-core.html
data SelectCore
  = -- | @SELECT ...@
    SelectCore'Select Select
  | -- | @VALUES ...@
    SelectCore'Values (NonEmpty (NonEmpty Expression))

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

-- |
-- * https://www.sqlite.org/syntax/join-clause.html
-- * https://www.sqlite.org/syntax/table-or-subquery.html
data Table
  = Table QualifiedTableName
  | Table'CrossJoin Table Table (Maybe JoinConstraint)
  | Table'Function (Aliased (FunctionCall NonEmpty))
  | Table'InnerJoin Table Table (Maybe JoinConstraint)
  | Table'LeftOuterJoin Table Table (Maybe JoinConstraint)
  | Table'NaturalCrossJoin Table Table
  | Table'NaturalInnerJoin Table Table
  | Table'NaturalLeftOuterJoin Table Table
  | Table'Subquery (Aliased SelectStatement)

newtype WindowClause
  = WindowClause (NonEmpty (Text, WindowDefinition))

data WithClause = WithClause
  { -- | @RECURSIVE@
    recursive :: Bool,
    commonTableExpressions :: NonEmpty CommonTableExpression
  }
