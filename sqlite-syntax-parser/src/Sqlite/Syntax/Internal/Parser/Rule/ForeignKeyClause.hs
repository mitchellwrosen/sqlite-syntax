module Sqlite.Syntax.Internal.Parser.Rule.ForeignKeyClause
  ( foreignKeyClauseRule,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.ForeignKeyClause
import qualified Sqlite.Syntax.Parser.Token as Token
import Sqlite.Syntax.Token (Token)
import qualified Sqlite.Syntax.Token as Token
import Prelude

foreignKeyClauseRule :: forall r. Rule r ForeignKeyClause
foreignKeyClauseRule =
  (\table columns (onDelete, onUpdate) deferred -> ForeignKeyClause {table, columns, onDelete, onUpdate, deferred})
    <$> (Token.references *> Token.identifier)
    <*> optional (parens (commaSep1 Token.identifier))
    <*> (fromActions <$> many actionClauseRule)
    <*> deferredRule
  where
    fromActions :: [Maybe (Either Action Action)] -> (Action, Action)
    fromActions =
      foldr (\x (y, z) -> maybe (y, z) (either (,z) (y,)) x) (NoAction, NoAction)

actionRule :: Rule r Action
actionRule =
  choice
    [ Cascade <$ Token.cascade,
      NoAction <$ (Token.no *> Token.action),
      Restrict <$ Token.restrict,
      SetDefault <$ (Token.set *> Token.default_),
      SetNull <$ (Token.set *> Token.null)
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
