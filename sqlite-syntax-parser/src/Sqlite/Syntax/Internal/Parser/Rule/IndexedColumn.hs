module Sqlite.Syntax.Internal.Parser.Rule.IndexedColumn
  ( indexedColumnRule,
  )
where

import Control.Applicative (optional)
import Sqlite.Syntax.Internal.Parser.Rule.Ordering (orderingRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.IndexedColumn (IndexedColumn (..))
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

-- | https://sqlite.org/syntax/indexed-column.html
indexedColumnRule :: Rule r IndexedColumn
indexedColumnRule =
  IndexedColumn
    <$> Token.identifier
    <*> optional (Token.collate *> Token.identifier)
    <*> orderingRule
