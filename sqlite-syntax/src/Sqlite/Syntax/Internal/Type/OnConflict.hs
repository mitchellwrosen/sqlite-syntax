-- TODO rename module
module Sqlite.Syntax.Internal.Type.OnConflict
  ( ConflictResolution (..),
  )
where

import GHC.Generics (Generic)
import Prelude

-- | https://sqlite.org/lang_conflict.html
data ConflictResolution
  = Abort
  | Fail
  | Ignore
  | Replace
  | Rollback
  deriving stock (Eq, Generic, Show)
