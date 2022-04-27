module Sqlite.Syntax.Internal.Parser.Rule.LiteralValue
  ( literalValueRule,
  )
where

import Control.Applicative.Combinators (choice)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Utils
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude hiding (Ordering, fail, lex, not, null)

literalValueRule :: Rule r LiteralValue
literalValueRule =
  choice
    [ Blob <$> Token.blob,
      Boolean False <$ Token.false,
      Boolean True <$ Token.true,
      CurrentDate <$ Token.currentDate,
      CurrentTime <$ Token.currentTime,
      CurrentTimestamp <$ Token.currentTimestamp,
      Null <$ Token.null,
      Number <$> Token.number,
      String <$> Token.string
    ]
