module Sqlite.Syntax.Internal.Type.SelectStatement where

import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Aliased (Aliased)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.FunctionCall
import Sqlite.Syntax.Internal.Type.Namespaced
import Sqlite.Syntax.Internal.Type.OrderingTerm
import Sqlite.Syntax.Internal.Type.QualifiedTableName
import Sqlite.Syntax.Internal.Type.Window
import Prelude hiding (Ordering, fail, not, null)

-- | https://sqlite.org/syntax/common-table-expression.html
--
-- @
-- __table__ [(__column__+)] /AS/ [[/NOT/] /MATERIALIZED/] (__select-statement__)
-- @
data CommonTableExpression = CommonTableExpression
  { table :: Text,
    columns :: Maybe (NonEmpty Text),
    materialized :: Maybe Bool,
    select :: SelectStatement
  }
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- /WITH/ [/RECURSIVE/] __common-table-expression__+
-- @
--
-- TODO move
data CommonTableExpressions = CommonTableExpressions
  { recursive :: Bool,
    tables :: NonEmpty CommonTableExpression
  }
  deriving stock (Eq, Generic, Show)

-- |
--
-- @
-- __select-core__
-- __compound-select__ /EXCEPT/ __select-core__
-- __compound-select__ /INTERSECT/ __select-core__
-- __compound-select__ /UNION/ __select-core__
-- __compound-select__ /UNION ALL/ __select-core__
-- @
--
-- idea: use this type internally for Earley parsing, but then convert to simpler recursive tree structure after?
data CompoundSelect
  = -- | TODO rename constructor
    CompoundSelect SelectCore
  | Except CompoundSelect SelectCore
  | Intersect CompoundSelect SelectCore
  | Union CompoundSelect SelectCore
  | UnionAll CompoundSelect SelectCore
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- /GROUP BY/ __expression__+ [/HAVING/ expression]
-- @
data GroupBy = GroupBy
  { groupBy :: NonEmpty Expression,
    having :: Maybe Expression
  }
  deriving stock (Eq, Generic, Show)

-- | https://www.sqlite.org/syntax/join-constraint.html
--
-- @
-- /ON/ __expression__
-- /USING/ __column__+
-- @
data JoinConstraint
  = On Expression
  | Using (NonEmpty Text)
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- /LIMIT/ __expression__ [/OFFSET/ __expression__]
-- @
data Limit = Limit
  { limit :: Expression,
    offset :: Maybe Expression
  }
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/result-column.html
--
-- @
-- __expression__ [/AS/] __column__
-- [__table__.]__*__
-- @
data ResultColumn
  = ResultColumn'Expression (Aliased Maybe Expression)
  | ResultColumn'Wildcard (Namespaced Text ())
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- /SELECT/ [/DISTINCT/] __result-column__+
--   [/FROM/ __table_]
--   [/WHERE/ __expression__]
--   [/GROUP BY/ __group-by__]
--   [/WINDOW/ __window__+]
-- @
data Select = Select
  { distinct :: Bool,
    columns :: NonEmpty ResultColumn,
    from :: Maybe Table,
    where_ :: Maybe Expression,
    groupBy :: Maybe GroupBy,
    window :: Maybe (NonEmpty (Aliased Identity Window))
  }
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/select-core.html
--
-- @
-- /SELECT/ __select__
-- /VALUES/ (__expression__+)+
-- @
data SelectCore
  = SelectCore'Select Select
  | SelectCore'Values (NonEmpty (NonEmpty Expression))
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/select-stmt.html
--
-- @
-- [__common-table-expressions__]
--   __compound-select__
--   [/ORDER BY/ __ordering-term__+]
--   [__limit__]
-- @
data SelectStatement = SelectStatement
  { with :: Maybe CommonTableExpressions,
    select :: CompoundSelect,
    orderBy :: Maybe (NonEmpty OrderingTerm),
    limit :: Maybe Limit
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
