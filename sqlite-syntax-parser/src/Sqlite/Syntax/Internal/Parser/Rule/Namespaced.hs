module Sqlite.Syntax.Internal.Parser.Rule.Namespaced
  ( namespacedRule,
  )
where

import Control.Applicative (optional)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Namespaced
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

namespacedRule :: Rule r a -> Rule r b -> Rule r (Namespaced a b)
namespacedRule p1 p2 =
  Namespaced
    <$> optional (p1 <* Token.fullStop)
    <*> p2
