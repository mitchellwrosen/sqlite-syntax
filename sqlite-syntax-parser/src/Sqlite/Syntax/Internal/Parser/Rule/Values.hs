module Sqlite.Syntax.Internal.Parser.Rule.Values
  ( makeValuesRule,
  )
where

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.Values (Values (..))
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

makeValuesRule :: forall r. Rule r Expression -> Rule r Values
makeValuesRule expressionRule =
  coerce @(Rule r (NonEmpty (NonEmpty Expression))) do
    Token.values *> commaSep1 (parens (commaSep1 expressionRule))
