module Sqlite.Syntax.Internal.Parser.Rule.OrderingTerm
  ( makeOrderingTermRule,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Rule.Ordering (orderingRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.OrderingTerm
import qualified Sqlite.Syntax.Parser.Token as Token

makeOrderingTermRule :: Rule r Expression -> Rule r OrderingTerm
makeOrderingTermRule expressionRule =
  OrderingTerm
    <$> expressionRule
    <*> optional (Token.collate *> Token.identifier)
    <*> orderingRule
    <*> optional nullsWhichRule

nullsWhichRule :: Rule r NullsWhich
nullsWhichRule =
  choice
    [ NullsWhich'First <$ (Token.nulls *> Token.first),
      NullsWhich'Last <$ (Token.nulls *> Token.last)
    ]
