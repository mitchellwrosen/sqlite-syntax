module Sqlite.Syntax.Internal.Type.ForeignKeyClause
  ( Action (..),
    ForeignKeyClause (..),
    Reference (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Namespaced (Namespaced)
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
  { reference :: Namespaced Text Reference,
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
