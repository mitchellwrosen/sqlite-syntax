module Sqlite.Syntax.Internal.Parser.Rule.InsertStatement
  ( makeInsertStatementRule,
  )
where

import Control.Applicative (Alternative, many, optional)
import Control.Applicative.Combinators (choice)
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Sqlite.Syntax.Internal.Parser.Rule.Aliased (asAliasRule)
import Sqlite.Syntax.Internal.Parser.Rule.ConflictResolution (makeConflictResolutionRule)
import Sqlite.Syntax.Internal.Parser.Rule.IndexedColumn (indexedColumnRule)
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.CommonTableExpressions (CommonTableExpressions)
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.InsertStatement
import Sqlite.Syntax.Internal.Type.OnConflict (ConflictResolution (..))
import Sqlite.Syntax.Internal.Type.Returning (Returning)
import Sqlite.Syntax.Internal.Type.SelectStatement (SelectStatement)
import Sqlite.Syntax.Internal.Type.Values (Values)
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

-- | https://www.sqlite.org/syntax/insert-stmt.html
makeInsertStatementRule ::
  forall r.
  Rule r CommonTableExpressions ->
  Rule r Expression ->
  Rule r Returning ->
  Rule r SelectStatement ->
  Rule r Values ->
  Rule r InsertStatement
makeInsertStatementRule commonTableExpressionsRule expressionRule returningRule selectStatementRule valuesRule =
  InsertStatement
    <$> optional commonTableExpressionsRule
    <*> choice
      [ Abort <$ Token.insert,
        Replace <$ Token.replace,
        makeConflictResolutionRule (Token.insert *> Token.or)
      ]
    <*> (Token.into *> asAliasRule (namespacedRule Token.identifier Token.identifier))
    <*> optional (parens (commaSep1 Token.identifier))
    <*> insertRule
    <*> optional returningRule
  where
    conflictTargetRule :: Rule r ConflictTarget
    conflictTargetRule =
      makeConflictTargetRule expressionRule

    insertRule :: Rule r Insert
    insertRule =
      choice
        [ InsertDefaultValues <$ (Token.default_ *> Token.values),
          InsertSelect <$> selectStatementRule <*> optional upsertClausesRule,
          InsertValues <$> valuesRule <*> optional upsertClausesRule
        ]

    upsertClausesRule :: Rule r UpsertClauses
    upsertClausesRule =
      UpsertClauses
        <$> many (upsertClauseRule (fmap Identity))
        <*> upsertClauseRule optional

    upsertClauseRule ::
      (forall p a. Alternative p => p a -> p (f a)) ->
      Rule r (UpsertClause f)
    upsertClauseRule p =
      UpsertClause
        <$> (Token.on *> Token.conflict *> p conflictTargetRule)
        <*> (Token.do_ *> makeUpsertActionRule expressionRule)

makeConflictTargetRule :: Rule r Expression -> Rule r ConflictTarget
makeConflictTargetRule expressionRule =
  ConflictTarget
    <$> parens (commaSep1 indexedColumnRule)
    <*> optional (Token.where_ *> expressionRule)

makeUpsertActionRule :: Rule r Expression -> Rule r UpsertAction
makeUpsertActionRule expressionRule =
  choice
    [ DoNothing <$ Token.nothing,
      DoUpdate
        <$> (Token.update *> Token.set *> commaSep1 ((,) <$> columnNamesRule <*> (Token.equalsSign *> expressionRule)))
        <*> optional (Token.where_ *> expressionRule)
    ]
  where
    columnNamesRule :: Rule r (NonEmpty Text)
    columnNamesRule =
      choice
        [ NonEmpty.singleton <$> Token.identifier,
          parens (commaSep1 Token.identifier)
        ]
