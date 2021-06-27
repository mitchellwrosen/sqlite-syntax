module Sqlite.Syntax.Internal.Parser.Rule.SelectStatement
  ( makeSelectStatementRule,
  )
where

import Control.Applicative (optional)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Utils
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude hiding (Ordering, fail, lex, not, null)

makeLimitRule :: Rule r Expression -> Rule r Limit
makeLimitRule expressionRule =
  munge
    <$> (Token.limit *> expressionRule)
    <*> optional
      ( choice
          [ Left <$> (Token.offset *> expressionRule),
            Right <$> (Token.comma *> expressionRule)
          ]
      )
  where
    munge :: Expression -> Maybe (Either Expression Expression) -> Limit
    munge e1 = \case
      -- LIMIT x
      Nothing -> Limit e1 Nothing
      -- LIMIT x OFFSET y
      Just (Left e2) -> Limit e1 (Just e2)
      -- LIMIT y, x
      Just (Right e2) -> Limit e2 (Just e1)

makeSelectStatementRule ::
  forall r.
  Rule r CommonTableExpressions ->
  Rule r CompoundSelect ->
  Rule r Expression ->
  Rule r OrderingTerm ->
  Rule r SelectStatement
makeSelectStatementRule commonTableExpressionsRule compoundSelectRule expressionRule orderingTermRule =
  SelectStatement
    <$> optional commonTableExpressionsRule
    <*> compoundSelectRule
    <*> optional (Token.order *> Token.by *> commaSep1 orderingTermRule)
    <*> optional (makeLimitRule expressionRule)
