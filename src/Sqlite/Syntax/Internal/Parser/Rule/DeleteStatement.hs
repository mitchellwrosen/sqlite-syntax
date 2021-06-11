module Sqlite.Syntax.Internal.Parser.Rule.DeleteStatement
  ( makeDeleteStatementRule,
  )
where

import Control.Applicative (optional)
import Sqlite.Syntax.Internal.Parser.Rule.QualifiedTableName (qualifiedTableNameRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.CommonTableExpressions
import Sqlite.Syntax.Internal.Type.DeleteStatement
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.Returning (Returning)
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

makeDeleteStatementRule ::
  Rule r CommonTableExpressions ->
  Rule r Expression ->
  Rule r Returning ->
  Rule r DeleteStatement
makeDeleteStatementRule commonTableExpressionsRule expressionRule returningRule =
  DeleteStatement
    <$> optional commonTableExpressionsRule
    <*> (Token.delete *> Token.from *> qualifiedTableNameRule)
    <*> optional (Token.where_ *> expressionRule)
    <*> optional returningRule
