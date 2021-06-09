module Sqlite.Syntax.Internal.Parser.Rule.Ordering
  ( orderingRule,
  )
where

import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Ordering (Ordering (..))
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude hiding (Ordering)

orderingRule :: Rule r Ordering
orderingRule =
  choice
    [ Ascending <$ perhaps_ Token.asc,
      Descending <$ Token.desc
    ]
