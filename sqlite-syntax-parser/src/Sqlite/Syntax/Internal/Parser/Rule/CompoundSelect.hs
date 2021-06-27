module Sqlite.Syntax.Internal.Parser.Rule.CompoundSelect
  ( makeCompoundSelectRule,
  )
where

import Control.Applicative.Combinators (choice)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Utils
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude hiding (Ordering, fail, lex, not, null)

makeCompoundSelectRule :: Rule r CompoundSelect -> Rule r SelectCore -> Rule r CompoundSelect
makeCompoundSelectRule compoundSelectRule selectCoreRule =
  choice
    [ CompoundSelect <$> selectCoreRule,
      Except <$> compoundSelectRule <*> (Token.except *> selectCoreRule),
      Intersect <$> compoundSelectRule <*> (Token.intersect *> selectCoreRule),
      Union <$> compoundSelectRule <*> (Token.union *> selectCoreRule),
      UnionAll <$> compoundSelectRule <*> (Token.union *> Token.all *> selectCoreRule)
    ]
