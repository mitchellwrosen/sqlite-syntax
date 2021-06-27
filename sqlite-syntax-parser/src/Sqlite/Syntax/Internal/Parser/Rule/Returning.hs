module Sqlite.Syntax.Internal.Parser.Rule.Returning
  ( makeReturningRule,
  )
where

import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Rule.Aliased (asAliasRule')
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.Returning
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

-- | https://sqlite.org/syntax/returning-clause.html
makeReturningRule :: Rule r Expression -> Rule r Returning
makeReturningRule expressionRule =
  Token.returning
    *> choice
      [ Returning <$> commaSep1 (asAliasRule' expressionRule),
        ReturningAll <$ Token.asterisk
      ]
