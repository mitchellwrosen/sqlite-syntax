module Sqlite.Syntax.Internal.Parser.Rule.QualifiedTableName
  ( qualifiedTableNameRule,
  )
where

import Control.Applicative (optional)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Rule.Aliased (asAliasRule)
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.QualifiedTableName
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

-- | https://sqlite.org/syntax/qualified-table-name.html
qualifiedTableNameRule :: Rule r QualifiedTableName
qualifiedTableNameRule =
  QualifiedTableName
    <$> asAliasRule (namespacedRule Token.identifier Token.identifier)
    <*> optional indexedByRule

indexedByRule :: Rule r IndexedBy
indexedByRule =
  choice
    [ IndexedBy <$> (Token.indexed *> Token.by *> Token.identifier),
      NotIndexed <$ (Token.not *> Token.indexed)
    ]
