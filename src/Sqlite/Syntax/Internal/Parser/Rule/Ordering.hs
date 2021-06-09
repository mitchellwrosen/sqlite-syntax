module Sqlite.Syntax.Internal.Parser.Rule.Ordering
  ( orderingRule,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.OrderingTerm
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude hiding (Ordering)

orderingRule :: Rule r Ordering
orderingRule =
  choice
    [ Ordering'Asc <$ perhaps_ Token.asc,
      Ordering'Desc <$ Token.desc
    ]
