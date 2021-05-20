module Sqlite.Syntax.Parser where

import Control.Applicative
import Control.Applicative.Combinators (choice)
import Data.Text (Text)
import qualified Sqlite.Syntax.Lexer as Lexer
import qualified Text.Earley as Earley
import Prelude

type Parser r =
  Earley.Prod r Text Lexer.Token

add :: Parser r Lexer.Token
add =
  Earley.token Lexer.ADD

alter :: Parser r Lexer.Token
alter =
  Earley.token Lexer.ALTER

column :: Parser r Lexer.Token
column =
  Earley.token Lexer.COLUMN

drop :: Parser r Lexer.Token
drop =
  Earley.token Lexer.DROP

fullStop :: Parser r Lexer.Token
fullStop =
  Earley.token Lexer.FullStop

identifier :: Parser r Text
identifier =
  Earley.terminal \case
    Lexer.Identifier s -> Just s
    _ -> Nothing

rename :: Parser r Lexer.Token
rename =
  Earley.token Lexer.RENAME

table :: Parser r Lexer.Token
table =
  Earley.token Lexer.TABLE

to :: Parser r Lexer.Token
to =
  Earley.token Lexer.TO

--

-- | https://sqlite.org/lang_altertable.html
data AlterTableStatement
  = AlterTableStatement (Maybe SchemaName) TableName TableAlteration

makeAlterTableStatement :: Parser r ColumnDefinition -> Parser r AlterTableStatement
makeAlterTableStatement columnDefinition =
  AlterTableStatement
    <$> (alter *> table *> optional schemaName <* fullStop)
    <*> tableName
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

-- TODO
data ColumnConstraint

data ColumnDefinition
  = ColumnDefinition ColumnName (Maybe TypeName) [ColumnConstraint]

newtype ColumnName
  = ColumnName Text

columnName :: Parser r ColumnName
columnName =
  ColumnName <$> identifier

newtype SchemaName
  = SchemaName Text

schemaName :: Parser r SchemaName
schemaName =
  SchemaName <$> identifier

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
