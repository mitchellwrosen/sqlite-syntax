-- TODO Rename to CommonTableExpressions
module Sqlite.Syntax.Internal.Parser.Rule.CommonTableExpression
  ( makeCommonTableExpressionsRule,
  )
where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.CommonTableExpressions (CommonTableExpression (..), CommonTableExpressions (..))
import Sqlite.Syntax.Internal.Type.SelectStatement (SelectStatement)
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude hiding (Ordering, fail, lex, not, null)

makeCommonTableExpressionsRule :: forall r. Rule r SelectStatement -> Rule r CommonTableExpressions
makeCommonTableExpressionsRule selectStatementRule =
  CommonTableExpressions <$> (Token.with *> perhaps Token.recursive) <*> commaSep1 commonTableExpressionRule
  where
    commonTableExpressionRule :: Rule r CommonTableExpression
    commonTableExpressionRule =
      CommonTableExpression
        <$> Token.identifier
        <*> optional (parens (commaSep1 Token.identifier))
        <*> ( Token.as
                *> optional
                  ( choice
                      [ False <$ (Token.not *> Token.materialized),
                        True <$ Token.materialized
                      ]
                  )
            )
        <*> parens selectStatementRule
