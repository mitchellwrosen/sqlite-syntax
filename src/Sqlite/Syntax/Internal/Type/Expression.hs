module Sqlite.Syntax.Internal.Type.Expression where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Sqlite.Syntax.Internal.Type.FilterClause (FilterClause)
import Sqlite.Syntax.Internal.Type.FunctionCall (FunctionCall)
import Sqlite.Syntax.Internal.Type.LiteralValue (LiteralValue)
import Sqlite.Syntax.Internal.Type.SchemaQualified (SchemaQualified)
import Sqlite.Syntax.Internal.Type.SelectStatement (SelectStatement)
import Sqlite.Syntax.Internal.Type.TableQualified (TableQualified)
import Prelude hiding (Ordering, fail, not, null)

-- TODO move this
data BindParameter

data CastExpression = CastExpression
  { expression :: Expression,
    type_ :: Text
  }

data CollateExpression = CollateExpression
  { expression :: Expression,
    collation :: Text
  }

-- TODO remove nots
data Expression
  = Expression'AggregateFunctionCall Text (Maybe FunctionArguments) (Maybe FilterClause)
  | Expression'And Expression Expression
  | Expression'Between Expression Expression Expression
  | Expression'BindParameter BindParameter
  | Expression'BitwiseAnd Expression Expression
  | Expression'BitwiseNegate Expression
  | Expression'BitwiseOr Expression Expression
  | Expression'Case (Maybe Expression) (NonEmpty (Expression, Expression)) (Maybe Expression)
  | Expression'Cast CastExpression
  | Expression'Collate CollateExpression
  | Expression'Column (SchemaQualified (TableQualified Text))
  | Expression'Concatenate Expression Expression
  | Expression'Divide Expression Expression
  | Expression'Equals Expression Expression
  | Expression'Exists SelectStatement
  | Expression'FunctionCall Text (Maybe FunctionArguments)
  | Expression'Glob Expression Expression (Maybe Expression)
  | Expression'GreaterThan Expression Expression
  | Expression'GreaterThanOrEquals Expression Expression
  | Expression'InFunction InFunctionExpression
  | Expression'InSubquery InSubqueryExpression
  | Expression'InTable InTableExpression
  | Expression'InValues InValuesExpression
  | Expression'Is Expression Expression
  | Expression'IsNot Expression Expression
  | Expression'LessThan Expression Expression
  | Expression'LessThanOrEquals Expression Expression
  | Expression'Like Expression Expression (Maybe Expression)
  | Expression'LiteralValue LiteralValue
  | Expression'Match Expression Expression (Maybe Expression)
  | Expression'Minus Expression Expression
  | Expression'Modulo Expression Expression
  | Expression'Multiply Expression Expression
  | Expression'Negate Expression
  | Expression'Not Expression
  | Expression'NotBetween Expression Expression Expression
  | Expression'NotEquals Expression Expression
  | Expression'NotGlob Expression Expression (Maybe Expression)
  | Expression'NotLike Expression Expression (Maybe Expression)
  | Expression'NotMatch Expression Expression (Maybe Expression)
  | Expression'NotRegexp Expression Expression (Maybe Expression)
  | Expression'Null
  | Expression'Or Expression Expression
  | Expression'Plus Expression Expression
  | Expression'RaiseFunction RaiseFunction
  | Expression'Regexp Expression Expression (Maybe Expression)
  | Expression'RowValue RowValue
  | Expression'ShiftLeft Expression Expression
  | Expression'ShiftRight Expression Expression
  | Expression'Subquery SelectStatement
  | Expression'WindowFunctionCall Text (Maybe FunctionArguments) (Maybe FilterClause) OverClause

data InFunctionExpression = InFunctionExpression
  { expression :: Expression,
    function :: FunctionCall []
  }

data InSubqueryExpression = InSubqueryExpression
  { expression :: Expression,
    subquery :: SelectStatement
  }

data InTableExpression = InTableExpression
  { expression :: Expression,
    table :: SchemaQualified Text
  }

data InValuesExpression = InValuesExpression
  { expression :: Expression,
    values :: [Expression]
  }

-- TODO break this up, distinct only belongs in agg
data FunctionArguments
  = FunctionArguments'Arguments Bool (NonEmpty Expression)
  | FunctionArguments'Wildcard

-- TODO move this
data OverClause
  = OverClause'WindowDefinition WindowDefinition
  | OverClause'WindowName Text

-- | https://sqlite.org/syntax/raise-function.html
-- TODO move this
data RaiseFunction
  = RaiseFunction'Abort Text
  | RaiseFunction'Fail Text
  | RaiseFunction'Ignore
  | RaiseFunction'Rollback Text

-- | https://www.sqlite.org/rowvalue.html
-- TODO move this
data RowValue
  = RowValue Expression Expression [Expression]

-- TODO move this
data WindowDefinition
