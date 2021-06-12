module Sqlite.Syntax.Internal.Parser.Rule.Columns
  ( columnRule,
    columnsRule,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Columns (Columns)
import Sqlite.Syntax.Internal.Type.Namespaced (Namespaced)
import qualified Sqlite.Syntax.Parser.Token as Token

-- | @schema.table.column @
columnRule :: forall r. Rule r (Columns Identity)
columnRule =
  namespacedRule tableRule (coerce @(Rule r Text) Token.identifier)

-- |
-- @
-- schema.table
-- schema.table (column, ...)
-- @
columnsRule :: Rule r (Columns [])
columnsRule =
  namespacedRule
    tableRule
    ( choice
        [ parens (NonEmpty.toList <$> commaSep1 Token.identifier),
          pure []
        ]
    )

-- | @schema.table@
tableRule :: Rule r (Namespaced Text Text)
tableRule =
  namespacedRule Token.identifier Token.identifier
