module Sqlite.Syntax.Internal.Type.ColumnDefinition
  ( ColumnConstraint (..),
    ColumnDefinition (..),
    Default (..),
    GeneratedType (..),
    Sign (..),
    SignedNumber (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.ForeignKeyClause (ForeignKeyClause)
import Sqlite.Syntax.Internal.Type.LiteralValue (LiteralValue)
import Sqlite.Syntax.Internal.Type.Named (Named)
import Sqlite.Syntax.Internal.Type.OnConflict (ConflictResolution)
import Sqlite.Syntax.Internal.Type.Ordering (Ordering)
import Prelude hiding (Ordering)

data ColumnConstraint
  = ColumnConstraint'Check Expression
  | ColumnConstraint'Collate Text
  | ColumnConstraint'Default Default
  | ColumnConstraint'ForeignKey ForeignKeyClause
  | ColumnConstraint'Generated Expression (Maybe GeneratedType)
  | ColumnConstraint'NotNull ConflictResolution
  | ColumnConstraint'PrimaryKey Ordering ConflictResolution Bool
  | ColumnConstraint'Unique ConflictResolution
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/column-def.html
data ColumnDefinition = ColumnDefinition
  { name :: Text,
    type_ :: Maybe Text,
    constraints :: [Named ColumnConstraint]
  }
  deriving stock (Eq, Generic, Show)

data Default
  = Default'Expression Expression
  | Default'LiteralValue LiteralValue
  | Default'SignedNumber SignedNumber
  deriving stock (Eq, Generic, Show)

data GeneratedType
  = Stored
  | Virtual
  deriving stock (Eq, Generic, Show)

data Sign
  = Sign'HyphenMinus
  | Sign'PlusSign
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/signed-number.html
data SignedNumber
  = SignedNumber (Maybe Sign) Text
  deriving stock (Eq, Generic, Show)
