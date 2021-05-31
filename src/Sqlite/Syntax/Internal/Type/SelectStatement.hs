module Sqlite.Syntax.Internal.Type.SelectStatement where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.OrderingTerm
import Sqlite.Syntax.Internal.Type.SchemaQualified
import Sqlite.Syntax.Internal.Type.TableQualified
import Prelude hiding (Ordering, fail, not, null)

-- | https://sqlite.org/syntax/common-table-expression.html
data CommonTableExpression = CommonTableExpression
  { table :: Text,
    columns :: Maybe (NonEmpty Text),
    materialized :: Maybe Bool,
    select :: SelectStatement
  }

data CompoundOperator
  = CompoundOperator'Except
  | CompoundOperator'Intersect
  | CompoundOperator'Union
  | CompoundOperator'UnionAll

data CompoundSelect
  = CompoundSelect'Compound CompoundSelect CompoundOperator SelectCore
  | CompoundSelect'Simple SelectCore

newtype FromClause
  = FromClause (Either (NonEmpty TableOrSubquery) JoinClause)

data GroupByClause = GroupByClause
  { groupBy :: NonEmpty Expression,
    having :: Maybe Expression
  }

data JoinClause

data LimitClause = LimitClause
  { limit :: Expression,
    offset :: Maybe Expression
  }

data Select = Select
  { distinct :: Bool,
    columns :: NonEmpty (SchemaQualified (TableQualified Text)),
    from :: Maybe FromClause,
    where_ :: Maybe Expression,
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
    with :: WithClause,
    -- | @SELECT ...@
    select :: CompoundSelect,
    -- | @ORDER BY ...@
    orderBy :: Maybe (NonEmpty OrderingTerm),
    -- | @LIMIT ...@
    limit :: Maybe LimitClause
  }

data TableOrSubquery

newtype WindowClause
  = WindowClause (NonEmpty (Text, WindowDefinition))

data WithClause = WithClause
  { -- | @RECURSIVE@
    recursive :: Bool,
    commonTableExpressions :: NonEmpty CommonTableExpression
  }
