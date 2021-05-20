module Sqlite.Syntax.Parser where

import Control.Applicative
import Control.Applicative.Combinators (choice)
import Data.Text (Text)
import qualified Sqlite.Syntax.Lexer as Lexer
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, not, null)

type Parser r =
  Earley.Prod r Text Lexer.Token

add :: Parser r Lexer.Token
add =
  Earley.token Lexer.ADD

alter :: Parser r Lexer.Token
alter =
  Earley.token Lexer.ALTER

always :: Parser r Lexer.Token
always =
  Earley.token Lexer.ALWAYS

analyze :: Parser r Lexer.Token
analyze =
  Earley.token Lexer.ANALYZE

as :: Parser r Lexer.Token
as =
  Earley.token Lexer.AS

asc :: Parser r Lexer.Token
asc =
  Earley.token Lexer.ASC

autoincrement :: Parser r Lexer.Token
autoincrement =
  Earley.token Lexer.AUTOINCREMENT

check :: Parser r Lexer.Token
check =
  Earley.token Lexer.CHECK

collate :: Parser r Lexer.Token
collate =
  Earley.token Lexer.COLLATE

column :: Parser r Lexer.Token
column =
  Earley.token Lexer.COLUMN

constraint :: Parser r Lexer.Token
constraint =
  Earley.token Lexer.CONSTRAINT

desc :: Parser r Lexer.Token
desc =
  Earley.token Lexer.DESC

drop :: Parser r Lexer.Token
drop =
  Earley.token Lexer.DROP

fullStop :: Parser r Lexer.Token
fullStop =
  Earley.token Lexer.FullStop

generated :: Parser r Lexer.Token
generated =
  Earley.token Lexer.GENERATED

identifier :: Parser r Text
identifier =
  Earley.terminal \case
    Lexer.Identifier s -> Just s
    _ -> Nothing

key :: Parser r Lexer.Token
key =
  Earley.token Lexer.KEY

leftParenthesis :: Parser r Lexer.Token
leftParenthesis =
  Earley.token Lexer.LeftParenthesis

not :: Parser r Lexer.Token
not =
  Earley.token Lexer.NOT

null :: Parser r Lexer.Token
null =
  Earley.token Lexer.NULL

primary :: Parser r Lexer.Token
primary =
  Earley.token Lexer.PRIMARY

rename :: Parser r Lexer.Token
rename =
  Earley.token Lexer.RENAME

rightParenthesis :: Parser r Lexer.Token
rightParenthesis =
  Earley.token Lexer.RightParenthesis

stored :: Parser r Lexer.Token
stored =
  Earley.token Lexer.STORED

table :: Parser r Lexer.Token
table =
  Earley.token Lexer.TABLE

to :: Parser r Lexer.Token
to =
  Earley.token Lexer.TO

unique :: Parser r Lexer.Token
unique =
  Earley.token Lexer.UNIQUE

virtual :: Parser r Lexer.Token
virtual =
  Earley.token Lexer.VIRTUAL

--

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
  | ColumnConstraintType'NotNull ConflictClause
  | ColumnConstraintType'PrimaryKey (Maybe Ordering) ConflictClause Bool
  | ColumnConstraintType'Unique ConflictClause

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
        <$> (not *> null *> conflictClause),
      ColumnConstraintType'PrimaryKey
        <$> (primary *> key *> optional ordering)
        <*> conflictClause
        <*> (True <$ autoincrement <|> pure False),
      ColumnConstraintType'Unique
        <$> (unique *> conflictClause)
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

conflictClause :: Parser r ConflictClause
conflictClause = undefined

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

literalValue :: Parser r LiteralValue
literalValue = undefined

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

data SignedNumber

signedNumber :: Parser r SignedNumber
signedNumber = undefined

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
