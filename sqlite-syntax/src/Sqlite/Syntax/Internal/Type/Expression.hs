module Sqlite.Syntax.Internal.Type.Expression where

import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Sqlite.Syntax.Internal.Type.FunctionCall (FunctionCall)
import Sqlite.Syntax.Internal.Type.LiteralValue (LiteralValue)
import Sqlite.Syntax.Internal.Type.Namespaced (Namespaced)
import Sqlite.Syntax.Internal.Type.SelectStatement (SelectStatement)
import Sqlite.Syntax.Internal.Type.Window (Window)
import Prelude

-- | https://sqlite.org/lang_aggfunc.html
--
-- An aggregate function call that takes a single argument, preceded by the keyword @DISTINCT@.
--
--   * /In any aggregate function that takes a single argument, that argument can be preceded by the keyword DISTINCT. In such cases, duplicate elements are filtered before being passed into the aggregate function. For example, the function "count(distinct X)" will return the number of distinct values of column X instead of the total number of non-null values in column X./
--
-- @
-- ∙(DISTINCT ∙) FILTER (WHERE ∙)
-- @
data AggregateDistinctFunctionCallExpression = AggregateDistinctFunctionCallExpression
  { -- | /∙(DISTINCT ∙)/
    call :: FunctionCall Identity,
    -- | /FILTER (WHERE ∙)/
    filter :: Maybe Expression
  }
  deriving stock (Eq, Generic, Show)

-- TODO move this
data Parameter
  = -- | /:NAME, \@NAME, $NAME
    Parameter'Named Text
  | -- | /?, ?N/
    Parameter'Ordinal (Maybe Natural)
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/lang_expr.html#the_case_expression
--
-- @
-- CASE ∙
--   WHEN ∙ THEN ∙
--   WHEN ∙ THEN ∙
--   ELSE ∙
-- END
-- @
data CaseExpression = CaseExpression
  { -- | /CASE ∙/
    base :: Maybe Expression,
    -- | /WHEN ∙ THEN ∙/
    cases :: NonEmpty (Expression, Expression),
    -- | /ELSE ∙/
    else_ :: Expression
  }
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/lang_expr.html#cast_expressions
--
-- @
-- CAST (∙ AS ∙)
-- @
data CastExpression = CastExpression
  { expression :: Expression,
    type_ :: Text
  }
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- ∙ COLLATE ∙
-- @
data CollateExpression = CollateExpression
  { expression :: Expression,
    collation :: Text
  }
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/lang_expr.html
data Expression
  = -- | /∙(DISTINCT ∙) FILTER ∙/
    Expression'AggregateDistinctFunctionCall AggregateDistinctFunctionCallExpression
  | -- | /∙ AND ∙/
    Expression'And Expression Expression
  | -- | /∙ BETWEEN ∙ AND ∙/
    Expression'Between Expression Expression Expression
  | -- | /∙ & ∙/
    Expression'BitwiseAnd Expression Expression
  | -- | /∙ ~ ∙/
    Expression'BitwiseNegate Expression
  | -- | /∙ | ∙/
    Expression'BitwiseOr Expression Expression
  | -- | /CASE ∙ WHEN ∙ THEN ∙ ELSE ∙ END/
    Expression'Case CaseExpression
  | -- | /CAST (∙ AS ∙)/
    Expression'Cast CastExpression
  | -- | /∙ COLLATE ∙/
    Expression'Collate CollateExpression
  | -- | /∙.∙.∙/
    Expression'Column (Namespaced (Namespaced Text Text) Text)
  | -- | /∙ || ∙/
    Expression'Concatenate Expression Expression
  | -- | /∙ / ∙/
    Expression'Divide Expression Expression
  | -- | /∙ = ∙, ∙ == ∙/
    Expression'Equals Expression Expression
  | -- | /EXISTS ∙/
    Expression'Exists SelectStatement
  | -- | /∙(∙) FILTER (WHERE ∙) OVER ∙/
    Expression'FunctionCall FunctionCallExpression
  | -- | /∙ GLOB ∙ ESCAPE ∙/
    Expression'Glob Expression Expression (Maybe Expression)
  | -- | /∙ > ∙/
    Expression'GreaterThan Expression Expression
  | -- | /∙ >= ∙/
    Expression'GreaterThanOrEquals Expression Expression
  | -- | /∙ IN ∙(∙)/
    Expression'InFunction InFunctionExpression
  | -- | /∙ IN (∙)/
    Expression'InSubquery InSubqueryExpression
  | -- | /∙ IN ∙/
    Expression'InTable InTableExpression
  | -- | /∙ IN (∙)/
    Expression'InValues InValuesExpression
  | -- | /∙ IS ∙/
    Expression'Is Expression Expression
  | -- | /∙ < ∙/
    Expression'LessThan Expression Expression
  | -- | /∙ <= ∙/
    Expression'LessThanOrEquals Expression Expression
  | -- | /∙ LIKE ∙ ESCAPE ∙/
    Expression'Like Expression Expression (Maybe Expression)
  | Expression'LiteralValue LiteralValue
  | -- | /∙ MATCH ∙ ESCAPE ∙/
    Expression'Match Expression Expression (Maybe Expression)
  | -- | /∙ - ∙/
    Expression'Minus Expression Expression
  | -- | /∙ % ∙/
    Expression'Modulo Expression Expression
  | -- | /∙ * ∙/
    Expression'Multiply Expression Expression
  | -- | /- ∙/
    Expression'Negate Expression
  | -- | /NOT ∙/
    Expression'Not Expression
  | -- | /∙ != ∙, ∙ <> ∙/
    Expression'NotEquals Expression Expression
  | -- | /∙ OR ∙/
    Expression'Or Expression Expression
  | Expression'Parameter Parameter
  | -- | /∙ + ∙/
    Expression'Plus Expression Expression
  | Expression'Raise Raise
  | -- | /∙ MATCH ∙ ESCAPE ∙/
    Expression'Regexp Expression Expression (Maybe Expression)
  | Expression'RowValue RowValue
  | -- | /∙ << ∙/
    Expression'ShiftLeft Expression Expression
  | -- | /∙ >> ∙/
    Expression'ShiftRight Expression Expression
  | -- | /(∙)/
    Expression'Subquery SelectStatement
  deriving stock (Eq, Generic, Show)

data FunctionArguments expression
  = FunctionArguments'Arguments [expression]
  | FunctionArguments'Wildcard
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/lang_expr.html#functions
--
-- @
-- ∙(∙) FILTER (WHERE ∙) OVER ∙
-- @
data FunctionCallExpression = FunctionCallExpression
  { -- | /∙(∙)/
    call :: FunctionCall FunctionArguments,
    -- | /FILTER (WHERE ∙)/
    filter :: Maybe Expression,
    -- | /OVER ∙/
    over :: Maybe Over
  }
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- ∙ IN ∙(∙)
-- @
data InFunctionExpression = InFunctionExpression
  { expression :: Expression,
    function :: FunctionCall []
  }
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- ∙ IN (∙)
-- @
data InSubqueryExpression = InSubqueryExpression
  { expression :: Expression,
    subquery :: SelectStatement
  }
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- ∙ IN ∙
-- @
data InTableExpression = InTableExpression
  { expression :: Expression,
    table :: Namespaced Text Text
  }
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- ∙ IN (∙)
-- @
data InValuesExpression = InValuesExpression
  { expression :: Expression,
    values :: [Expression]
  }
  deriving stock (Eq, Generic, Show)

-- TODO rename?
data Over
  = Over'Window Window
  | Over'WindowName Text
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
