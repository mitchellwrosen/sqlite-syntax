module Sqlite.Syntax.Parser where

import Control.Applicative
import Control.Applicative.Combinators (choice)
import Data.Text (Text)
import Sqlite.Syntax.Parser.Token
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, not, null)

parens :: Parser r a -> Parser r a
parens p =
  leftParenthesis *> p <* rightParenthesis

--

data Syntax
  = Syntax'AlterTableStatement AlterTableStatement
  | Syntax'AnalyzeStatement AnalyzeStatement

syntax :: Earley.Grammar r (Parser r Syntax)
syntax = mdo
  let alterTableStatement = makeAlterTableStatement columnDefinition
  let columnDefinition = undefined
  pure do
    choice
      [ Syntax'AlterTableStatement <$> alterTableStatement,
        Syntax'AnalyzeStatement <$> analyzeStatement
      ]

--

-- | https://sqlite.org/lang_altertable.html
data AlterTableStatement
  = AlterTableStatement (SchemaQualified TableName) TableAlteration

makeAlterTableStatement :: Parser r ColumnDefinition -> Parser r AlterTableStatement
makeAlterTableStatement columnDefinition =
  AlterTableStatement
    <$> (alter *> table *> schemaQualified tableName)
    <*> choice
      [ TableAlteration'AddColumn
          <$> columnDefinition,
        TableAlteration'DropColumn
          <$> columnName,
        TableAlteration'Rename
          <$> (rename *> to *> tableName),
        TableAlteration'RenameColumn
          <$> (rename *> optional column *> columnName)
          <*> (to *> columnName)
      ]

-- | https://sqlite.org/lang_analyze.html
data AnalyzeStatement
  = AnalyzeStatement (Maybe (SchemaQualified IndexOrTableName))

analyzeStatement :: Parser r AnalyzeStatement
analyzeStatement =
  AnalyzeStatement
    <$> (analyze *> optional (schemaQualified indexOrTableName))

-- | https://sqlite.org/syntax/column-constraint.html
data ColumnConstraint
  = ColumnConstraint (Maybe ConstraintName) ColumnConstraintType

makeColumnConstraint :: Parser r ColumnConstraintType -> Parser r ColumnConstraint
makeColumnConstraint columnConstraintType =
  ColumnConstraint
    <$> optional (constraint *> constraintName)
    <*> columnConstraintType

data ColumnConstraintType
  = ColumnConstraintType'Check Expression
  | ColumnConstraintType'Collate CollationName
  | ColumnConstraintType'Default Default
  | ColumnConstraintType'ForeignKey ForeignKeyClause
  | ColumnConstraintType'Generated Expression (Maybe GeneratedType)
  | ColumnConstraintType'NotNull (Maybe ConflictClause)
  | ColumnConstraintType'PrimaryKey (Maybe Ordering) (Maybe ConflictClause) Bool
  | ColumnConstraintType'Unique (Maybe ConflictClause)

makeColumnConstraintType :: Parser r Expression -> Parser r ColumnConstraintType
makeColumnConstraintType expression =
  choice
    [ ColumnConstraintType'Check
        <$> (check *> parens expression),
      ColumnConstraintType'Collate
        <$> (collate *> collationName),
      ColumnConstraintType'Default
        <$> choice
          [ Default'Expression <$> parens expression,
            Default'LiteralValue <$> literalValue,
            Default'SignedNumber <$> signedNumber
          ],
      ColumnConstraintType'ForeignKey
        <$> foreignKeyClause,
      ColumnConstraintType'Generated
        <$> (optional (generated *> always) *> as *> parens expression)
        <*> optional generatedType,
      ColumnConstraintType'NotNull
        <$> (not *> null *> optional conflictClause),
      ColumnConstraintType'PrimaryKey
        <$> (primary *> key *> optional ordering)
        <*> optional conflictClause
        <*> (True <$ autoincrement <|> pure False),
      ColumnConstraintType'Unique
        <$> (unique *> optional conflictClause)
    ]

-- | https://sqlite.org/syntax/column-def.html
data ColumnDefinition
  = ColumnDefinition ColumnName (Maybe TypeName) [ColumnConstraint]

makeColumnDefinition :: Parser r ColumnConstraint -> Parser r ColumnDefinition
makeColumnDefinition columnConstraint =
  ColumnDefinition
    <$> columnName
    <*> optional typeName
    <*> many columnConstraint

newtype CollationName
  = CollationName Text

collationName :: Parser r CollationName
collationName =
  CollationName <$> undefined

newtype ColumnName
  = ColumnName Text

columnName :: Parser r ColumnName
columnName =
  ColumnName <$> identifier

-- | https://sqlite.org/syntax/conflict-clause.html
data ConflictClause
  = ConflictClause'Abort
  | ConflictClause'Fail
  | ConflictClause'Ignore
  | ConflictClause'Replace
  | ConflictClause'Rollback

conflictClause :: Parser r ConflictClause
conflictClause =
  on
    *> conflict
    *> choice
      [ ConflictClause'Abort <$ abort,
        ConflictClause'Fail <$ fail,
        ConflictClause'Ignore <$ ignore,
        ConflictClause'Replace <$ replace,
        ConflictClause'Rollback <$ rollback
      ]

newtype ConstraintName
  = ConstraintName Text

constraintName :: Parser r ConstraintName
constraintName =
  ConstraintName <$> identifier

data Default
  = Default'Expression Expression
  | Default'LiteralValue LiteralValue
  | Default'SignedNumber SignedNumber

data Expression

makeExpression :: Parser r Expression
makeExpression = undefined

data ForeignKeyClause

foreignKeyClause :: Parser r ForeignKeyClause
foreignKeyClause = undefined

data GeneratedType
  = GeneratedType'Stored
  | GeneratedType'Virtual

generatedType :: Parser r GeneratedType
generatedType =
  choice
    [ GeneratedType'Stored <$ stored,
      GeneratedType'Virtual <$ virtual
    ]

newtype IndexOrTableName
  = IndexOrTableName Text

indexOrTableName :: Parser r IndexOrTableName
indexOrTableName =
  IndexOrTableName <$> identifier

data LiteralValue
  = LiteralValue'BlobLiteral Text
  | LiteralValue'CurrentDate
  | LiteralValue'CurrentTime
  | LiteralValue'CurrentTimestamp
  | LiteralValue'False
  | LiteralValue'Null
  | LiteralValue'NumericLiteral Text
  | LiteralValue'StringLiteral Text
  | LiteralValue'True

literalValue :: Parser r LiteralValue
literalValue =
  choice
    [ LiteralValue'BlobLiteral <$> blob,
      LiteralValue'CurrentDate <$ currentDate,
      LiteralValue'CurrentTime <$ currentTime,
      LiteralValue'CurrentTimestamp <$ currentTimestamp,
      LiteralValue'False <$ false,
      LiteralValue'Null <$ null,
      LiteralValue'NumericLiteral <$> number,
      LiteralValue'StringLiteral <$> string,
      LiteralValue'True <$ true
    ]

data Ordering
  = Ordering'Asc
  | Ordering'Desc

ordering :: Parser r Ordering
ordering =
  choice
    [ Ordering'Asc <$ asc,
      Ordering'Desc <$ desc
    ]

newtype SchemaName
  = SchemaName Text

schemaName :: Parser r SchemaName
schemaName =
  SchemaName <$> identifier

data SchemaQualified a
  = SchemaQualified (Maybe SchemaName) a

schemaQualified :: Parser r a -> Parser r (SchemaQualified a)
schemaQualified p =
  SchemaQualified
    <$> (optional schemaName <* fullStop)
    <*> p

data Sign
  = Sign'HyphenMinus
  | Sign'PlusSign

sign :: Parser r Sign
sign =
  choice
    [ Sign'HyphenMinus <$ hyphenMinus,
      Sign'PlusSign <$ plusSign
    ]

-- | https://sqlite.org/syntax/signed-number.html
data SignedNumber
  = SignedNumber (Maybe Sign) Text

signedNumber :: Parser r SignedNumber
signedNumber =
  SignedNumber
    <$> optional sign
    <*> number

data TableAlteration
  = TableAlteration'AddColumn ColumnDefinition
  | TableAlteration'DropColumn ColumnName
  | TableAlteration'Rename TableName
  | TableAlteration'RenameColumn ColumnName ColumnName

newtype TableName
  = TableName Text

tableName :: Parser r TableName
tableName =
  TableName <$> identifier

newtype TypeName
  = TypeName Text

typeName :: Parser r TypeName
typeName =
  TypeName <$> identifier
