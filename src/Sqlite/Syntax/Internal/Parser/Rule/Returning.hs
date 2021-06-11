module Sqlite.Syntax.Internal.Parser.Rule.Returning
  ( makeReturningRule,
  )
where

import Control.Applicative (optional)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Aliased (Aliased (..))
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.Returning
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

makeReturningRule :: Rule r Expression -> Rule r Returning
makeReturningRule expressionRule =
  Token.returning
    *> choice
      [ Returning <$> commaSep1 (Aliased <$> expressionRule <*> optional (perhaps_ Token.as *> Token.identifier)),
        ReturningAll <$ Token.asterisk
      ]
