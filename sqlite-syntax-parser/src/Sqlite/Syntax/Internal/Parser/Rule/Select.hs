module Sqlite.Syntax.Internal.Parser.Rule.Select
  ( makeSelectRule,
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
import Prelude hiding (Ordering, fail, lex, not, null)

makeSelectRule ::
  forall r.
  Rule r Expression ->
  Rule r Table ->
  Rule r Window ->
  Rule r Select
makeSelectRule expressionRule tableRule windowRule =
  Select
    <$> distinctRule
    <*> commaSep1 resultColumnRule
    <*> optional (Token.from *> tableRule)
    <*> optional (Token.where_ *> expressionRule)
    <*> optional groupByRule
    <*> optional windowClauseRule
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
    -- https://www.sqlite.org/syntax/result-column.html
    resultColumnRule :: Rule r ResultColumn
    resultColumnRule =
      choice
        [ ResultColumn'Expression <$> asAliasRule' expressionRule,
          ResultColumn'Wildcard <$> namespacedRule Token.identifier (() <$ Token.asterisk)
        ]
    windowClauseRule :: Rule r (NonEmpty (Aliased Identity Window))
    windowClauseRule =
      Token.window *> commaSep1 (aliasAsRule windowRule)
