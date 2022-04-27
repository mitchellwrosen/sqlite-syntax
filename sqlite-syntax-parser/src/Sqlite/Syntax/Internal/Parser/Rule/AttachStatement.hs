module Sqlite.Syntax.Internal.Parser.Rule.AttachStatement
  ( makeAttachStatementRule,
  )
where

import Control.Applicative (optional)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Utils
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude hiding (Ordering, fail, lex, not, null)

makeAttachStatementRule :: Rule r Expression -> Rule r AttachStatement
makeAttachStatementRule expression = do
  AttachStatement <$> (Token.attach *> optional Token.database *> expression) <*> (Token.as *> Token.identifier)
