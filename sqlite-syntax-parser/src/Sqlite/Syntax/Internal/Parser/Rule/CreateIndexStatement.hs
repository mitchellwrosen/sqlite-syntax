module Sqlite.Syntax.Internal.Parser.Rule.CreateIndexStatement
  ( makeCreateIndexStatementRule,
  )
where

import Control.Applicative (optional)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Rule.IndexedColumn (indexedColumnRule)
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

makeCreateIndexStatementRule :: Rule r Expression -> Rule r CreateIndexStatement
makeCreateIndexStatementRule expressionRule =
  CreateIndexStatement
    <$> (Token.create *> perhaps Token.unique)
    <*> (Token.index *> perhaps (Token.if_ *> Token.not *> Token.exists))
    <*> namespacedRule Token.identifier Token.identifier
    <*> (Token.on *> Token.identifier)
    <*> parens (commaSep1 indexedColumnRule)
    <*> optional (Token.where_ *> expressionRule)
