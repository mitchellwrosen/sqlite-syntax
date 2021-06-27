module Sqlite.Syntax.Internal.Parser.Rule.Aliased
  ( aliasAsRule,
    asAliasRule,
    asAliasRule',
  )
where

import Control.Applicative (optional)
import Data.Functor.Identity (Identity (..))
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Aliased (Aliased (..))
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

-- | @<alias> AS <thing>@
aliasAsRule :: Rule r a -> Rule r (Aliased Identity a)
aliasAsRule p =
  flip Aliased <$> (Identity <$> Token.identifier) <*> (Token.as *> p)

-- | @<thing> [AS <alias>]@
asAliasRule :: Rule r a -> Rule r (Aliased Maybe a)
asAliasRule p =
  Aliased <$> p <*> optional (Token.as *> Token.identifier)

-- | @<thing> [[AS] <alias>]@
asAliasRule' :: Rule r a -> Rule r (Aliased Maybe a)
asAliasRule' p =
  Aliased <$> p <*> optional (perhaps_ Token.as *> Token.identifier)
