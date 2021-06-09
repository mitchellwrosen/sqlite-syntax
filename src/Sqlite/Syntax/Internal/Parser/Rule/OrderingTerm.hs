module Sqlite.Syntax.Internal.Parser.Rule.OrderingTerm
  ( makeOrderingTermRule,
  )
where

import Control.Applicative (optional)
import Control.Applicative.Combinators (choice)
import Data.Maybe (fromMaybe)
import Sqlite.Syntax.Internal.Parser.Rule.Ordering (orderingRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.Ordering (Ordering (..))
import Sqlite.Syntax.Internal.Type.OrderingTerm
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

makeOrderingTermRule :: Rule r Expression -> Rule r OrderingTerm
makeOrderingTermRule expressionRule =
  ( \expression collation ordering maybeNullsWhich ->
      OrderingTerm
        { expression,
          collation,
          ordering,
          nullsPlacement =
            fromMaybe
              ( case ordering of
                  Ascending -> NullsFirst
                  Descending -> NullsLast
              )
              maybeNullsWhich
        }
  )
    <$> expressionRule
    <*> optional (Token.collate *> Token.identifier)
    <*> orderingRule
    <*> optional nullsPlacementRule

nullsPlacementRule :: Rule r NullsPlacement
nullsPlacementRule =
  Token.nulls
    *> choice
      [ NullsFirst <$ Token.first,
        NullsLast <$ Token.last
      ]
