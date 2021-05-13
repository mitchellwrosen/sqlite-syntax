-- | https://sqlite.org/syntax.html
module Lib where

import Control.Monad
import Data.Char
import Data.Coerce
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as List1
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (Ordering)

type List1 =
  List1.NonEmpty

type Parser a =
  Parsec Void Text a

data S2 a b
  = S2'1 a
  | S2'2 b
  deriving stock (Eq, Show)

data SepBy1 a b
  = SepBy1'Cons a b (SepBy1 a b)
  | SepBy1'End a
  deriving stock (Eq, Show)

--

-- TODO
data AggregateFunction

-- | @
-- DISTINCT? expression (, expression)*
-- @
--
-- FIXME distinct only makes sense for a single argument
data AggregateFunctionArguments = AggregateFunctionArguments
  { distinct :: Bool,
    arguments :: List1 Expression
  }

aggregateFunctionArgumentsParser :: Parser AggregateFunctionArguments
aggregateFunctionArgumentsParser = do
  distinct <- True <$ keyword "DISTINCT" <|> pure False
  arguments <- commaSep1 expressionParser
  pure AggregateFunctionArguments {distinct, arguments}

-- | https://www.sqlite.org/syntax/aggregate-function-invocation.html
data AggregateFunctionInvocation = AggregateFunctionInvocation
  { aggregateFunction :: Identifier,
    -- | @Nothing@ = @()@
    -- @Just Nothing@ = @(*)@
    arguments :: Maybe (Maybe AggregateFunctionArguments),
    filterClause :: Maybe FilterClause
  }

aggregateFunctionInvocationParser :: Parser AggregateFunctionInvocation
aggregateFunctionInvocationParser = do
  aggregateFunction <- identifierParser
  arguments <- parens (optional (Nothing <$ (char '*' *> space) <|> Just <$> aggregateFunctionArgumentsParser))
  filterClause <- optional filterClauseParser
  pure AggregateFunctionInvocation {aggregateFunction, arguments, filterClause}

data AlterTableStatement = AlterTableStatement
  { schemaName :: Maybe SchemaName,
    tableName :: TableName,
    tableAlteration :: TableAlteration
  }
  deriving stock (Eq, Show)

alterTableStatementParser :: Parser AlterTableStatement
alterTableStatementParser = do
  keyword "ALTER"
  keyword "TABLE"
  schemaName <- optional (schemaNameParser <* char '.')
  tableName <- tableNameParser
  tableAlteration <- tableAlterationParser
  pure AlterTableStatement {schemaName, tableName, tableAlteration}

-- TODO
data BlobLiteral
  = BlobLiteral
  deriving stock (Eq, Show)

-- TODO
data CollationName
  = CollationName
  deriving stock (Eq, Show)

-- TODO
data ColumnAlias
  = ColumnAlias
  deriving stock (Eq, Show)

-- TODO
data ColumnDefinition
  = ColumnDefinition
  deriving stock (Eq, Show)

-- TODO
data ColumnName
  = ColumnName
  deriving stock (Eq, Show)

data CommonTableExpression = CommonTableExpression
  { tableName :: TableName,
    columnNames :: [ColumnName],
    select :: SelectStatement
  }
  deriving stock (Eq, Show)

commonTableExpressionParser :: Parser CommonTableExpression
commonTableExpressionParser = undefined

data CompoundOperator
  = CompoundOperator'Except
  | CompoundOperator'Intersect
  | CompoundOperator'Union
  | CompoundOperator'UnionAll
  deriving stock (Eq, Show)

data Expression
  = Expression'Literal LiteralValue
  | -- | Expression'BindParameter BindParameter
    -- | Expression'Identifier (Maybe (Maybe SchemaName, TableName)) ColumnName
    -- | Expression'UnaryOperator UnaryOperator Expression
    -- | Expression'BinaryOperator Expression BinaryOperator Expression
    Expression'Function -- TODO
    -- TODO
  deriving stock (Eq, Show)

expressionParser :: Parser Expression
expressionParser =
  undefined

-- TODO
data FilterClause

filterClauseParser :: Parser FilterClause
filterClauseParser = undefined

data GroupBy = GroupBy
  { expressions :: List1 Expression,
    having :: Maybe Expression
  }
  deriving stock (Eq, Show)

newtype Identifier = Identifier {unIdentifier :: Text}
  deriving stock (Eq, Show)

-- | FIXME find the docs on what makes a valid identifier
identifierParser :: Parser Identifier
identifierParser = do
  ident <- takeWhile1P Nothing isAlpha
  pure (Identifier ident)

data Limit = Limit
  { expression :: Expression,
    offset :: Maybe Expression
  }
  deriving stock (Eq, Show)

data LiteralValue
  = LiteralValue'BlobLiteral BlobLiteral
  | LiteralValue'CurrentDate
  | LiteralValue'CurrentTime
  | LiteralValue'CurrentTimestamp
  | LiteralValue'False
  | LiteralValue'Null
  | LiteralValue'NumericLiteral NumericLiteral
  | LiteralValue'StringLiteral StringLiteral
  | LiteralValue'True
  deriving stock (Eq, Show)

-- TODO
data NumericLiteral
  = NumericLiteral
  deriving stock (Eq, Show)

data Ordering
  = Ordering'Ascending
  | Ordering'Descrnding
  deriving stock (Eq, Show)

data OrderingNulls
  = OrderingNulls'First
  | OrderingNulls'Last
  deriving stock (Eq, Show)

data OrderingTerm = OrderingTerm
  { expression :: Expression,
    collation :: Maybe CollationName,
    ordering :: Maybe Ordering,
    nulls :: Maybe OrderingNulls
  }
  deriving stock (Eq, Show)

data ResultColumn
  = ResultColumn'Expression Expression (Maybe ColumnAlias)
  | ResultColumn'Star (Maybe TableName)
  deriving stock (Eq, Show)

newtype SchemaName = SchemaName {unSchemaName :: Identifier}
  deriving stock (Eq, Show)

schemaNameParser :: Parser SchemaName
schemaNameParser =
  coerce identifierParser

data SelectStatement = SelectStatement
  { with :: Maybe With,
    selects :: SepBy1 (S2 SelectCore Values) CompoundOperator,
    orderBy :: [OrderingTerm],
    limit :: Maybe Limit
  }
  deriving stock (Eq, Show)

data SelectCore = SelectCore
  { distinct :: Bool,
    resultColumns :: List1 ResultColumn,
    from :: [TableOrSubquery],
    where_ :: Maybe Expression,
    groupBy :: Maybe GroupBy,
    window :: [Window]
  }
  deriving stock (Eq, Show)

-- TODO
data StringLiteral
  = StringLiteral
  deriving stock (Eq, Show)

data TableAlteration
  = TableAlteration'AddColumn ColumnDefinition
  | TableAlteration'DropColumn ColumnName
  | TableAlteration'RenameColumn ColumnName ColumnName
  | TableAlteration'RenameTable TableName
  deriving stock (Eq, Show)

tableAlterationParser :: Parser TableAlteration
tableAlterationParser =
  addColumnParser <|> dropColumnParser <|> renameColumnParser <|> renameTableParser
  where
    addColumnParser :: Parser TableAlteration
    addColumnParser =
      undefined

    dropColumnParser :: Parser TableAlteration
    dropColumnParser =
      undefined

    renameColumnParser :: Parser TableAlteration
    renameColumnParser =
      undefined

    renameTableParser :: Parser TableAlteration
    renameTableParser =
      undefined

newtype TableName = TableName {unTableName :: Identifier}
  deriving stock (Eq, Show)

tableNameParser :: Parser TableName
tableNameParser =
  coerce identifierParser

-- TODO
data TableOrSubquery
  = TableOrSubquery
  deriving stock (Eq, Show)

newtype Values
  = Values (List1 (List1 Expression))
  deriving stock (Eq, Show)

data Window
  = Window WindowName WindowDefinition
  deriving stock (Eq, Show)

-- TODO
data WindowDefinition
  = WindowDefinition
  deriving stock (Eq, Show)

-- TODO
data WindowName
  = WindowName
  deriving stock (Eq, Show)

data With = With
  { recursive :: Bool,
    commonTableExpressions :: List1 CommonTableExpression
  }
  deriving stock (Eq, Show)

withParser :: Parser With
withParser = do
  keyword "WITH"
  recursive <- (True <$ keyword "RECURSIVE" <|> pure False)
  commonTableExpressions <- commaSep1 commonTableExpressionParser
  pure With {commonTableExpressions, recursive}

--

debugParse :: Show a => Parser a -> Text -> IO ()
debugParse parser input =
  case parse ((,) <$> parser <*> takeRest) "" input of
    Left err -> putStrLn (errorBundlePretty err)
    Right (value, leftovers) -> do
      print value
      Text.putStrLn leftovers

commaSep1 :: Parser a -> Parser (List1 a)
commaSep1 parser =
  List1.fromList <$> sepBy1 parser (char ',' <* space)

parens :: Parser a -> Parser a
parens =
  between (char '(' *> space) (char ')' *> space)

keyword :: Text -> Parser ()
keyword word = do
  word' <- takeP Nothing (Text.length word)
  when (Text.toUpper word' /= word) (failure Nothing (Set.singleton (Label (List1.fromList (Text.unpack word)))))
  space1

keywords :: HashSet Text
keywords =
  HashSet.fromList
    [ "ABORT",
      "ACTION",
      "ADD",
      "AFTER",
      "ALL",
      "ALTER",
      "ALWAYS",
      "ANALYZE",
      "AND",
      "AS",
      "ASC",
      "ATTACH",
      "AUTOINCREMENT",
      "BEFORE",
      "BEGIN",
      "BETWEEN",
      "BY",
      "CASCADE",
      "CASE",
      "CAST",
      "CHECK",
      "COLLATE",
      "COLUMN",
      "COMMIT",
      "CONFLICT",
      "CONSTRAINT",
      "CREATE",
      "CROSS",
      "CURRENT",
      "CURRENT_DATE",
      "CURRENT_TIME",
      "CURRENT_TIMESTAMP",
      "DATABASE",
      "DEFAULT",
      "DEFERRABLE",
      "DEFERRED",
      "DELETE",
      "DESC",
      "DETACH",
      "DISTINCT",
      "DO",
      "DROP",
      "EACH",
      "ELSE",
      "END",
      "ESCAPE",
      "EXCEPT",
      "EXCLUDE",
      "EXCLUSIVE",
      "EXISTS",
      "EXPLAIN",
      "FAIL",
      "FILTER",
      "FIRST",
      "FOLLOWING",
      "FOR",
      "FOREIGN",
      "FROM",
      "FULL",
      "GENERATED",
      "GLOB",
      "GROUP",
      "GROUPS",
      "HAVING",
      "IF",
      "IGNORE",
      "IMMEDIATE",
      "IN",
      "INDEX",
      "INDEXED",
      "INITIALLY",
      "INNER",
      "INSERT",
      "INSTEAD",
      "INTERSECT",
      "INTO",
      "IS",
      "ISNULL",
      "JOIN",
      "KEY",
      "LAST",
      "LEFT",
      "LIKE",
      "LIMIT",
      "MATCH",
      "MATERIALIZED",
      "NATURAL",
      "NO",
      "NOT",
      "NOTHING",
      "NOTNULL",
      "NULL",
      "NULLS",
      "OF",
      "OFFSET",
      "ON",
      "OR",
      "ORDER",
      "OTHERS",
      "OUTER",
      "OVER",
      "PARTITION",
      "PLAN",
      "PRAGMA",
      "PRECEDING",
      "PRIMARY",
      "QUERY",
      "RAISE",
      "RANGE",
      "RECURSIVE",
      "REFERENCES",
      "REGEXP",
      "REINDEX",
      "RELEASE",
      "RENAME",
      "REPLACE",
      "RESTRICT",
      "RETURNING",
      "RIGHT",
      "ROLLBACK",
      "ROW",
      "ROWS",
      "SAVEPOINT",
      "SELECT",
      "SET",
      "TABLE",
      "TEMP",
      "TEMPORARY",
      "THEN",
      "TIES",
      "TO",
      "TRANSACTION",
      "TRIGGER",
      "UNBOUNDED",
      "UNION",
      "UNIQUE",
      "UPDATE",
      "USING",
      "VACUUM",
      "VALUES",
      "VIEW",
      "VIRTUAL",
      "WHEN",
      "WHERE",
      "WINDOW",
      "WITH",
      "WITHOUT"
    ]
