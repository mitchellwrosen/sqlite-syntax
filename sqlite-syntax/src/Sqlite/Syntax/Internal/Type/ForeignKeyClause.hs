module Sqlite.Syntax.Internal.Type.ForeignKeyClause
  ( Action (..),
    ForeignKeyClause (..),
  )
where

import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Columns (Columns)
import Prelude

-- | https://sqlite.org/syntax/foreign-key-clause.html
data Action
  = -- | /CASCADE/
    Cascade
  | -- | /NO ACTION/
    NoAction
  | -- | /RESTRICT/
    Restrict
  | -- | /SET DEFAULT/
    SetDefault
  | -- | /SET NULL/
    SetNull
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/foreign-key-clause.html
--
-- TODO rename?
data ForeignKeyClause = ForeignKeyClause
  { references :: Columns Identity [],
    onDelete :: Action,
    onUpdate :: Action,
    deferred :: Bool
  }
  deriving stock (Eq, Generic, Show)
