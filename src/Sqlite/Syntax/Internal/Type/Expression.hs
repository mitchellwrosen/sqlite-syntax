module Sqlite.Syntax.Internal.Type.Expression where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.FunctionCall (FunctionCall)
import Sqlite.Syntax.Internal.Type.LiteralValue (LiteralValue)
import Sqlite.Syntax.Internal.Type.SchemaQualified (SchemaQualified)
import Sqlite.Syntax.Internal.Type.SelectStatement (SelectStatement)
import Sqlite.Syntax.Internal.Type.TableQualified (TableQualified)
import Sqlite.Syntax.Internal.Type.Window (Window)
import Prelude hiding (Ordering, fail, not, null)

data AggregateFunctionCall = AggregateFunctionCall
  { call :: FunctionCall AggregateFunctionArguments,
    filter :: Maybe Expression
  }
  deriving stock (Eq, Generic, Show)

-- TODO move this
data BindParameter
  = BindParameter'TODO
  deriving stock (Eq, Generic, Show)

-- | @CASE ... WHEN ... THEN ... ELSE ... END@
data CaseExpression = CaseExpression
  { base :: Maybe Expression,
    cases :: NonEmpty (Expression, Expression),
    else_ :: Expression
  }
  deriving stock (Eq, Generic, Show)

data CastExpression = CastExpression
  { expression :: Expression,
    type_ :: Text
  }
  deriving stock (Eq, Generic, Show)

data CollateExpression = CollateExpression
  { expression :: Expression,
    collation :: Text
  }
  deriving stock (Eq, Generic, Show)

data Expression
  = Expression'AggregateFunctionCall AggregateFunctionCall
  | -- | @... AND ...@
    Expression'And Expression Expression
  | -- | @... BETWEEN ... AND ...@
    Expression'Between Expression Expression Expression
  | Expression'BindParameter BindParameter
  | -- | @... & ...@
    Expression'BitwiseAnd Expression Expression
  | -- | @... ~ ...@
    Expression'BitwiseNegate Expression
  | -- | @... | ...@
    Expression'BitwiseOr Expression Expression
  | -- | @CASE ... WHEN ... THEN ... ELSE ... END@
    Expression'Case CaseExpression
  | -- | @CAST (... AS ...)@
    Expression'Cast CastExpression
  | -- | @... COLLATE ...@
    Expression'Collate CollateExpression
  | -- | @ [[schema.]table.]column@
    Expression'Column (SchemaQualified (TableQualified Text))
  | -- | @... || ...@
    Expression'Concatenate Expression Expression
  | -- | @... / ...@
    Expression'Divide Expression Expression
  | -- | @... = ...@, @... == ...@
    Expression'Equals Expression Expression
  | -- | @EXISTS ...@
    Expression'Exists SelectStatement
  | Expression'FunctionCall (FunctionCall FunctionArguments)
  | Expression'Glob Expression Expression (Maybe Expression)
  | -- | @... > ...@
    Expression'GreaterThan Expression Expression
  | -- | @... >= ...@
    Expression'GreaterThanOrEquals Expression Expression
  | Expression'InFunction InFunctionExpression
  | Expression'InSubquery InSubqueryExpression
  | Expression'InTable InTableExpression
  | Expression'InValues InValuesExpression
  | -- | @... IS ...@
    Expression'Is Expression Expression
  | -- | @... < ...@
    Expression'LessThan Expression Expression
  | -- | @... <= ...@
    Expression'LessThanOrEquals Expression Expression
  | Expression'Like Expression Expression (Maybe Expression)
  | Expression'LiteralValue LiteralValue
  | Expression'Match Expression Expression (Maybe Expression)
  | -- | @... - ...@
    Expression'Minus Expression Expression
  | -- | @... % ...@
    Expression'Modulo Expression Expression
  | -- | @... * ...@
    Expression'Multiply Expression Expression
  | -- | @- ...@
    Expression'Negate Expression
  | -- | @NOT ...@
    Expression'Not Expression
  | -- | @... != ...@, @... <> ...@
    Expression'NotEquals Expression Expression
  | -- | @... OR ...@
    Expression'Or Expression Expression
  | -- | @... + ...@
    Expression'Plus Expression Expression
  | Expression'Raise Raise
  | Expression'Regexp Expression Expression (Maybe Expression)
  | Expression'RowValue RowValue
  | -- | @... << ...@
    Expression'ShiftLeft Expression Expression
  | -- | @... >> ...@
    Expression'ShiftRight Expression Expression
  | Expression'Subquery SelectStatement
  | Expression'WindowFunctionCall WindowFunctionCall
  deriving stock (Eq, Generic, Show)

data InFunctionExpression = InFunctionExpression
  { expression :: Expression,
    function :: FunctionCall []
  }
  deriving stock (Eq, Generic, Show)

data InSubqueryExpression = InSubqueryExpression
  { expression :: Expression,
    subquery :: SelectStatement
  }
  deriving stock (Eq, Generic, Show)

data InTableExpression = InTableExpression
  { expression :: Expression,
    table :: SchemaQualified Text
  }
  deriving stock (Eq, Generic, Show)

data InValuesExpression = InValuesExpression
  { expression :: Expression,
    values :: [Expression]
  }
  deriving stock (Eq, Generic, Show)

data FunctionArguments expression
  = FunctionArguments'Arguments [expression]
  | FunctionArguments'Wildcard
  deriving stock (Eq, Generic, Show)

data AggregateFunctionArguments expression
  = AggregateFunctionArguments'Arguments Bool (NonEmpty expression)
  | AggregateFunctionArguments'None
  | AggregateFunctionArguments'Wildcard
  deriving stock (Eq, Generic, Show)

-- TODO move this
data OverClause
  = OverClause'Window Window
  | OverClause'WindowName Text
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/raise-function.html
data Raise
  = Raise'Abort Text
  | Raise'Fail Text
  | Raise'Ignore
  | Raise'Rollback Text
  deriving stock (Eq, Generic, Show)

-- | https://www.sqlite.org/rowvalue.html
-- TODO move this
data RowValue
  = RowValue Expression Expression [Expression]
  deriving stock (Eq, Generic, Show)

data WindowFunctionCall = WindowFunctionCall
  { call :: FunctionCall FunctionArguments,
    filter :: Maybe Expression,
    over :: OverClause
  }
  deriving stock (Eq, Generic, Show)
