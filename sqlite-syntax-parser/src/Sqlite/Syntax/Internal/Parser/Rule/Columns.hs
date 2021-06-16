module Sqlite.Syntax.Internal.Parser.Rule.Columns
  ( columnsRule,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import qualified Data.List.NonEmpty as NonEmpty
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Columns (Columns)
import qualified Sqlite.Syntax.Parser.Token as Token

-- |
-- @
-- schema.table
-- schema.table (column, ...)
-- @
columnsRule :: (forall a. Rule r a -> Rule r (f a)) -> Rule r (Columns f [])
columnsRule p =
  namespacedRule
    (p (namespacedRule Token.identifier Token.identifier))
    ( choice
        [ parens (NonEmpty.toList <$> commaSep1 Token.identifier),
          pure []
        ]
    )
