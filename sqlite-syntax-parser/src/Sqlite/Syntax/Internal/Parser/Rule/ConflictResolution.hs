module Sqlite.Syntax.Internal.Parser.Rule.ConflictResolution
  ( makeConflictResolutionRule,
  )
where

import Control.Applicative.Combinators (choice)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.OnConflict (ConflictResolution (..))
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

makeConflictResolutionRule :: Rule r a -> Rule r ConflictResolution
makeConflictResolutionRule prefixRule =
  choice
    [ prefixRule
        *> choice
          [ Abort <$ Token.abort,
            Fail <$ Token.fail,
            Ignore <$ Token.ignore,
            Replace <$ Token.replace,
            Rollback <$ Token.rollback
          ],
      pure Abort
    ]
