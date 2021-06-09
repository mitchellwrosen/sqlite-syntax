module Sqlite.Syntax.Internal.Parser.Rule.OrderingTerm
  ( makeOrderingTermRule,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Data.Maybe (fromMaybe)
import Sqlite.Syntax.Internal.Parser.Rule.Ordering (orderingRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.OrderingTerm
import qualified Sqlite.Syntax.Parser.Token as Token

makeOrderingTermRule :: Rule r Expression -> Rule r OrderingTerm
makeOrderingTermRule expressionRule =
  ( \expression collation ordering maybeNullsWhich ->
      OrderingTerm
        { expression,
          collation,
          ordering,
          nullsWhich =
            fromMaybe
              ( case ordering of
                  Ordering'Asc -> NullsWhich'First
                  Ordering'Desc -> NullsWhich'Last
              )
              maybeNullsWhich
        }
  )
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
