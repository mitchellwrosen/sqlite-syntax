module Sqlite.Syntax.Internal.Type.ForeignKeyClause
  ( Action (..),
    ForeignKeyClause (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
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
  { table :: Text,
    columns :: Maybe (NonEmpty Text),
    onDelete :: Action,
    onUpdate :: Action,
    deferred :: Bool
  }
  deriving stock (Eq, Generic, Show)
