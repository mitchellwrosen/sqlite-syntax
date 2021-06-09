-- TODO Rename to CommonTableExpressions
module Sqlite.Syntax.Internal.Parser.Rule.Table
  ( makeTableRule,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Rule.FunctionCall (functionCallRule)
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Aliased
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.QualifiedTableName
import Sqlite.Syntax.Internal.Type.SelectStatement
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
          Table'Function
            <$> ( Aliased
                    <$> functionCallRule (commaSep1 expressionRule)
                    <*> optional (perhaps_ Token.as *> Token.identifier)
                ),
          Table'Subquery
            <$> (Aliased <$> parens selectStatementRule <*> optional (perhaps_ Token.as *> Token.identifier)),
          parens tableRule
        ]
  pure tableRule
  where
    joinConstraintRule :: Rule r JoinConstraint
    joinConstraintRule =
      choice
        [ JoinConstraint'On <$> (Token.on *> expressionRule),
          JoinConstraint'Using <$> (Token.using *> parens (commaSep1 Token.identifier))
        ]

qualifiedTableNameRule :: Rule r QualifiedTableName
qualifiedTableNameRule =
  QualifiedTableName
    <$> ( Aliased
            <$> namespacedRule Token.identifier Token.identifier
            <*> optional (Token.as *> Token.identifier)
        )
    <*> optional indexedByParser
  where
    indexedByParser :: Rule r IndexedBy
    indexedByParser =
      choice
        [ IndexedBy'IndexedBy <$> (Token.indexed *> Token.by *> Token.identifier),
          IndexedBy'NotIndexed <$ (Token.not *> Token.indexed)
        ]
