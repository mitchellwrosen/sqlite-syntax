module Sqlite.Syntax.Internal.Parser.Rule.SelectCore
  ( makeSelectCoreRule,
  )
where

import Control.Applicative.Combinators (choice)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Utils
import Prelude hiding (Ordering, fail, lex, not, null)

makeSelectCoreRule :: Rule r Select -> Rule r Values -> Rule r SelectCore
makeSelectCoreRule selectRule valuesRule =
  choice
    [ SelectCore'Select <$> selectRule,
      SelectCore'Values <$> valuesRule
    ]
