module Sqlite.Syntax.Internal.Parser.Rule.Table
  ( makeTableRule,
  )
where

import Control.Applicative (optional)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Rule.Aliased (asAliasRule')
import Sqlite.Syntax.Internal.Parser.Rule.FunctionCall (functionCallRule)
import Sqlite.Syntax.Internal.Parser.Rule.QualifiedTableName (qualifiedTableNameRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.SelectStatement (JoinConstraint (..), SelectStatement, Table (..))
import qualified Sqlite.Syntax.Parser.Token as Token
import qualified Text.Earley as Earley
import Prelude

makeTableRule :: forall r. Rule r Expression -> Rule r SelectStatement -> Earley.Grammar r (Rule r Table)
makeTableRule expressionRule selectStatementRule = mdo
  tableRule <-
    Earley.rule do
      choice
        [ Table'CrossJoin
            <$> tableRule
            <*> (Token.cross *> Token.join *> tableRule0)
            <*> optional joinConstraintRule,
          Table'InnerJoin
            <$> tableRule
            <*> (Token.comma *> tableRule0)
            <*> pure Nothing,
          Table'InnerJoin
            <$> tableRule
            <*> (perhaps Token.inner *> Token.join *> tableRule0)
            <*> optional joinConstraintRule,
          Table'LeftOuterJoin
            <$> tableRule
            <*> (Token.left *> perhaps Token.outer *> Token.join *> tableRule0)
            <*> optional joinConstraintRule,
          Table'NaturalCrossJoin
            <$> tableRule
            <*> (Token.natural *> Token.cross *> Token.join *> tableRule0),
          Table'NaturalInnerJoin
            <$> tableRule
            <*> (Token.natural *> perhaps Token.inner *> Token.join *> tableRule0),
          Table'NaturalLeftOuterJoin
            <$> tableRule
            <*> (Token.natural *> Token.left *> perhaps Token.outer *> Token.join *> tableRule0),
          tableRule0
        ]
  tableRule0 <-
    Earley.rule do
      choice
        [ Table <$> qualifiedTableNameRule,
          Table'Function <$> asAliasRule' (functionCallRule (commaSep1 expressionRule)),
          Table'Subquery <$> asAliasRule' (parens selectStatementRule),
          parens tableRule
        ]
  pure tableRule
  where
    joinConstraintRule :: Rule r JoinConstraint
    joinConstraintRule =
      choice
        [ On <$> (Token.on *> expressionRule),
          Using <$> (Token.using *> parens (commaSep1 Token.identifier))
        ]
