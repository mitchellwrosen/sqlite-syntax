module Lib where

import Control.Monad
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
  deriving stock (Show)

data SepBy1 a b
  = SepBy1'Cons a b (SepBy1 a b)
  | SepBy1'End a
  deriving stock (Show)

--

-- TODO
data BlobLiteral
  = BlobLiteral
  deriving stock (Show)

-- TODO
data CollationName
  = CollationName
  deriving stock (Show)

-- TODO
data ColumnAlias
  = ColumnAlias
  deriving stock (Show)

-- TODO
data ColumnName
  = ColumnName
  deriving stock (Show)

data CommonTableExpression = CommonTableExpression
  { tableName :: TableName,
    columnNames :: [ColumnName],
    select :: SelectStatement
  }
  deriving stock (Show)

commonTableExpressionParser :: Parser CommonTableExpression
commonTableExpressionParser = undefined

data CompoundOperator
  = CompoundOperator'Except
  | CompoundOperator'Intersect
  | CompoundOperator'Union
  | CompoundOperator'UnionAll
  deriving stock (Show)

data Expression
  = Expression'Literal LiteralValue
  | -- | Expression'BindParameter BindParameter
    -- | Expression'Identifier (Maybe (Maybe SchemaName, TableName)) ColumnName
    -- | Expression'UnaryOperator UnaryOperator Expression
    -- | Expression'BinaryOperator Expression BinaryOperator Expression
    Expression'Function -- TODO
    -- TODO
  deriving stock (Show)

data GroupBy = GroupBy
  { expressions :: List1 Expression,
    having :: Maybe Expression
  }
  deriving stock (Show)

data Limit = Limit
  { expression :: Expression,
    offset :: Maybe Expression
  }
  deriving stock (Show)

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
  deriving stock (Show)

-- TODO
data NumericLiteral
  = NumericLiteral
  deriving stock (Show)

data Ordering
  = Ordering'Ascending
  | Ordering'Descrnding
  deriving stock (Show)

data OrderingNulls
  = OrderingNulls'First
  | OrderingNulls'Last
  deriving stock (Show)

data OrderingTerm = OrderingTerm
  { expression :: Expression,
    collation :: Maybe CollationName,
    ordering :: Maybe Ordering,
    nulls :: Maybe OrderingNulls
  }
  deriving stock (Show)

data ResultColumn
  = ResultColumn'Expression Expression (Maybe ColumnAlias)
  | ResultColumn'Star (Maybe TableName)
  deriving stock (Show)

data SelectStatement = SelectStatement
  { with :: Maybe With,
    selects :: SepBy1 (S2 SelectCore Values) CompoundOperator,
    orderBy :: [OrderingTerm],
    limit :: Maybe Limit
  }
  deriving stock (Show)

data SelectCore = SelectCore
  { distinct :: Bool,
    resultColumns :: List1 ResultColumn,
    from :: [TableOrSubquery],
    where_ :: Maybe Expression,
    groupBy :: Maybe GroupBy,
    window :: [Window]
  }
  deriving stock (Show)

-- TODO
data StringLiteral
  = StringLiteral
  deriving stock (Show)

-- TODO
data TableName
  = TableName
  deriving stock (Show)

-- TODO
data TableOrSubquery
  = TableOrSubquery
  deriving stock (Show)

newtype Values
  = Values (List1 (List1 Expression))
  deriving stock (Show)

data Window
  = Window WindowName WindowDefinition
  deriving stock (Show)

-- TODO
data WindowDefinition
  = WindowDefinition
  deriving stock (Show)

-- TODO
data WindowName
  = WindowName
  deriving stock (Show)

data With = With
  { recursive :: Bool,
    commonTableExpressions :: List1 CommonTableExpression
  }
  deriving stock (Show)

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
