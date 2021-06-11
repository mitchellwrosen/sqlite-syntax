module Sqlite.Syntax.Internal.Type.Returning
  ( Returning (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Aliased (Aliased)
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Prelude

-- | https://sqlite.org/syntax/returning-clause.html
--
-- @
-- /RETURNING/ (__expression__ [/AS/ __alias])+
-- /RETURNING/ __\*__
-- @
data Returning
  = Returning (NonEmpty (Aliased Maybe Expression))
  | ReturningAll
  deriving stock (Eq, Generic, Show)
