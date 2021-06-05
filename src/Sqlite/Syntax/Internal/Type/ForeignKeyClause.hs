module Sqlite.Syntax.Internal.Type.ForeignKeyClause
  ( Action (..),
    ForeignKeyClause (..),
    Reference (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.SchemaQualified
import Prelude

data Action
  = -- | @CASCADE@
    Action'Cascade
  | -- | @NO ACTION@
    Action'NoAction
  | -- | @RESTRICT@
    Action'Restrict
  | -- | @SET DEFAULT@
    Action'SetDefault
  | -- | @SET NULL@
    Action'SetNull
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/foreign-key-clause.html
-- TODO rename?
data ForeignKeyClause = ForeignKeyClause
  { reference :: SchemaQualified Reference,
    onDelete :: Action,
    onUpdate :: Action,
    deferred :: Bool
  }
  deriving stock (Eq, Generic, Show)

data Reference = Reference
  { table :: Text,
    columns :: [Text]
  }
  deriving stock (Eq, Generic, Show)
