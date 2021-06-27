module Sqlite.Syntax.Internal.Parser.Rule.SelectStatement
  ( makeCompoundSelectRule,
    makeSelectStatementRule,
  )
where

import Control.Applicative (optional)
import Control.Applicative.Combinators (choice)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Rule.Aliased (aliasAsRule, asAliasRule')
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import qualified Sqlite.Syntax.Parser.Token as Token
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, lex, not, null)

-- TODO move into own module
makeCompoundSelectRule ::
  forall r.
  Rule r Expression ->
  Rule r Table ->
  Rule r Values ->
  Rule r Window ->
  Earley.Grammar r (Rule r CompoundSelect)
makeCompoundSelectRule expressionRule tableRule valuesRule windowRule = mdo
  compoundSelectRule <-
    Earley.rule do
      choice
        [ CompoundSelect <$> selectCoreRule,
          Except <$> compoundSelectRule <*> (Token.except *> selectCoreRule),
          Intersect <$> compoundSelectRule <*> (Token.intersect *> selectCoreRule),
          Union <$> compoundSelectRule <*> (Token.union *> selectCoreRule),
          UnionAll <$> compoundSelectRule <*> (Token.union *> Token.all *> selectCoreRule)
        ]
  selectRule <-
    Earley.rule do
      Select
        <$> distinctRule
        <*> commaSep1 (makeResultColumnRule expressionRule)
        <*> optional (Token.from *> tableRule)
        <*> optional (Token.where_ *> expressionRule)
        <*> optional groupByRule
        <*> optional windowClauseRule
  selectCoreRule <-
    Earley.rule do
      choice
        [ SelectCore'Select <$> selectRule,
          SelectCore'Values <$> valuesRule
        ]
  pure compoundSelectRule
  where
    distinctRule :: Rule r Bool
    distinctRule =
      Token.select
        *> choice
          [ True <$ Token.distinct,
            False <$ Token.all,
            pure False
          ]
    groupByRule :: Rule r GroupBy
    groupByRule =
      GroupBy
        <$> (Token.group *> Token.by *> commaSep1 expressionRule)
        <*> optional (Token.having *> expressionRule)
    windowClauseRule :: Rule r (NonEmpty (Aliased Identity Window))
    windowClauseRule =
      Token.window *> commaSep1 (aliasAsRule windowRule)

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

-- | https://www.sqlite.org/syntax/result-column.html
makeResultColumnRule :: Rule r Expression -> Rule r ResultColumn
makeResultColumnRule expressionRule =
  choice
    [ ResultColumn'Expression <$> asAliasRule' expressionRule,
      ResultColumn'Wildcard <$> namespacedRule Token.identifier (() <$ Token.asterisk)
    ]

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
