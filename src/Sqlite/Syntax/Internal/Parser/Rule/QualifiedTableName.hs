module Sqlite.Syntax.Internal.Parser.Rule.QualifiedTableName
  ( qualifiedTableNameRule,
  )
where

import Control.Applicative (optional)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Aliased (Aliased (..))
import Sqlite.Syntax.Internal.Type.QualifiedTableName
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

qualifiedTableNameRule :: Rule r QualifiedTableName
qualifiedTableNameRule =
  QualifiedTableName
    <$> ( Aliased
            <$> namespacedRule Token.identifier Token.identifier
            <*> optional (perhaps_ Token.as *> Token.identifier)
        )
    <*> optional indexedByParser
  where
    indexedByParser :: Rule r IndexedBy
    indexedByParser =
      choice
        [ IndexedBy'IndexedBy <$> (Token.indexed *> Token.by *> Token.identifier),
          IndexedBy'NotIndexed <$ (Token.not *> Token.indexed)
        ]
