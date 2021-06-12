module Sqlite.Syntax.Internal.Type.AlterTableStatement
  ( AlterTableStatement (..),
    TableAlteration (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.ColumnDefinition (ColumnDefinition)
import Sqlite.Syntax.Internal.Type.Namespaced (Namespaced)
import Prelude

-- | https://sqlite.org/lang_altertable.html
data AlterTableStatement = AlterTableStatement
  { table :: Namespaced Text Text,
    alteration :: TableAlteration
  }
  deriving stock (Eq, Generic, Show)

data TableAlteration
  = TableAlteration'AddColumn ColumnDefinition
  | TableAlteration'DropColumn Text
  | TableAlteration'Rename Text
  | TableAlteration'RenameColumn Text Text
  deriving stock (Eq, Generic, Show)
