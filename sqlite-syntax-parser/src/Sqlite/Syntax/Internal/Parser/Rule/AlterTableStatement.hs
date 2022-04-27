module Sqlite.Syntax.Internal.Parser.Rule.AlterTableStatement
  ( makeAlterTableStatementRule,
  )
where

import Control.Applicative (optional)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

makeAlterTableStatementRule :: Rule r ColumnDefinition -> Rule r AlterTableStatement
makeAlterTableStatementRule columnDefinitionRule =
  AlterTableStatement
    <$> (Token.alter *> Token.table *> namespacedRule Token.identifier Token.identifier)
    <*> choice
      [ TableAlteration'AddColumn <$> columnDefinitionRule,
        TableAlteration'DropColumn <$> Token.identifier,
        TableAlteration'Rename <$> (Token.rename *> Token.to *> Token.identifier),
        TableAlteration'RenameColumn
          <$> (Token.rename *> optional Token.column *> Token.identifier)
          <*> (Token.to *> Token.identifier)
      ]
