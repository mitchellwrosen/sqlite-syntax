module Sqlite.Syntax.Internal.Parser.Rule.ForeignKeyClause
  ( foreignKeyClauseRule,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import qualified Data.List.NonEmpty as NonEmpty
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.ForeignKeyClause
import qualified Sqlite.Syntax.Parser.Token as Token
import Sqlite.Syntax.Token (Token)
import qualified Sqlite.Syntax.Token as Token
import Prelude

foreignKeyClauseRule :: forall r. Rule r ForeignKeyClause
foreignKeyClauseRule =
  (\reference (onDelete, onUpdate) deferred -> ForeignKeyClause {reference, onDelete, onUpdate, deferred})
    <$> (Token.references *> namespacedRule Token.identifier referenceRule)
    <*> (fromActions <$> many actionClauseRule)
    <*> deferredRule
  where
    fromActions :: [Maybe (Either Action Action)] -> (Action, Action)
    fromActions =
      foldr (\x (y, z) -> maybe (y, z) (either (,z) (y,)) x) (Action'NoAction, Action'NoAction)

actionRule :: Rule r Action
actionRule =
  choice
    [ Action'Cascade <$ Token.cascade,
      Action'NoAction <$ (Token.no *> Token.action),
      Action'Restrict <$ Token.restrict,
      Action'SetDefault <$ (Token.set *> Token.default_),
      Action'SetNull <$ (Token.set *> Token.null)
    ]

actionClauseRule :: Rule r (Maybe (Either Action Action))
actionClauseRule =
  choice
    [ Just . Left <$> (Token.on *> Token.delete *> actionRule),
      Just . Right <$> (Token.on *> Token.update *> actionRule),
      Nothing <$ (Token.match *> Token.identifier)
    ]

deferredRule :: Rule r Bool
deferredRule =
  choice
    [ f
        <$> perhaps Token.not
        <*> (Token.deferrable *> optional (Token.initially *> choice [Token.deferred, Token.immediate])),
      pure False
    ]
  where
    f :: Bool -> Maybe Token -> Bool
    f False (Just Token.DEFERRED) = True
    f _ _ = False

referenceRule :: Rule r Reference
referenceRule =
  Reference
    <$> Token.identifier
    <*> choice
      [ parens (NonEmpty.toList <$> commaSep1 Token.identifier),
        pure []
      ]
