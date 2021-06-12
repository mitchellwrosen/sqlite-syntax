module Sqlite.Syntax.Internal.Type.OnConflict
  ( OnConflict (..),
  )
where

import GHC.Generics (Generic)
import Prelude

-- | https://sqlite.org/syntax/conflict-clause.html
data OnConflict
  = OnConflictAbort
  | OnConflictFail
  | OnConflictIgnore
  | OnConflictReplace
  | OnConflictRollback
  deriving stock (Eq, Generic, Show)
