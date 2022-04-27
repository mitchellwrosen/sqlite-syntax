module Sqlite.Syntax.Internal.Parser.Rule.AnalyzeStatement
  ( analyzeStatementRule,
  )
where

import Control.Applicative (optional)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Utils
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude hiding (Ordering, fail, lex, not, null)

analyzeStatementRule :: Rule r AnalyzeStatement
analyzeStatementRule =
  AnalyzeStatement <$> (Token.analyze *> optional (namespacedRule Token.identifier Token.identifier))
