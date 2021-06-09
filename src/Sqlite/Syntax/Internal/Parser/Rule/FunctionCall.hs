module Sqlite.Syntax.Internal.Parser.Rule.FunctionCall
  ( functionCallRule,
  )
where

import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.FunctionCall
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

functionCallRule :: Rule r (f Expression) -> Rule r (FunctionCall f)
functionCallRule arguments =
  FunctionCall
    <$> namespacedRule Token.identifier Token.identifier
    <*> parens arguments
