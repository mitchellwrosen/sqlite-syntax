module Sqlite.Syntax.Internal.Type.SelectStatement where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.SchemaQualified
import Sqlite.Syntax.Internal.Type.TableQualified
import Prelude hiding (Ordering, fail, not, null)

data CommonTableExpression

data CompoundOperator
  = CompoundOperator'Except
  | CompoundOperator'Intersect
  | CompoundOperator'Union
  | CompoundOperator'UnionAll

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

data OrderingTerm

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
  = SelectCore'Select Select
  | SelectCore'Values (NonEmpty (NonEmpty Expression))

-- | https://sqlite.org/syntax/select-stmt.html
data SelectStatement = SelectStatement
  { with :: WithClause,
    select :: (SelectCore, [(CompoundOperator, SelectCore)]),
    orderBy :: Maybe (NonEmpty OrderingTerm),
    limit :: Maybe LimitClause
  }

data TableOrSubquery

newtype WindowClause
  = WindowClause (NonEmpty (Text, WindowDefinition))

data WithClause = WithClause
  { recursive :: Bool,
    commonTableExpressions :: NonEmpty CommonTableExpression
  }
