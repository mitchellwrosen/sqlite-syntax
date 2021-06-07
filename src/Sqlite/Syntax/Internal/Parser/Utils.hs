module Sqlite.Syntax.Internal.Parser.Utils
  ( Rule,
    commaSep0,
    commaSep1,
    parens,
    perhaps,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Sqlite.Syntax.Parser.Token as Token
import Sqlite.Syntax.Token (LocatedToken)
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, lex, not, null)

type Rule r =
  Earley.Prod r Text LocatedToken

commaSep0 :: Rule r a -> Rule r [a]
commaSep0 p =
  choice
    [ NonEmpty.toList <$> commaSep1 p,
      pure []
    ]

commaSep1 :: Rule r a -> Rule r (NonEmpty a)
commaSep1 p =
  (:|) <$> p <*> many (Token.comma *> p)

parens :: Rule r a -> Rule r a
parens p =
  Token.leftParenthesis *> p <* Token.rightParenthesis

perhaps :: Rule r a -> Rule r Bool
perhaps p =
  choice [True <$ p, pure False]
