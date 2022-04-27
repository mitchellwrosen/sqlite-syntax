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

-- | https://sqlite.org/syntax/column-constraint.html
data ColumnConstraint
  = -- | /CHECK (∙)/
    ColumnConstraint'Check Expression
  | -- | /COLLATE ∙/
    ColumnConstraint'Collate Text
  | -- | /DEFAULT ∙/
    ColumnConstraint'Default Default
  | ColumnConstraint'ForeignKey ForeignKeyClause
  | -- | /GENERATED ALWAYS AS (∙) ∙/
    ColumnConstraint'Generated Expression GeneratedType
  | -- | /NOT NULL ∙/
    ColumnConstraint'NotNull ConflictResolution
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

-- | /DEFAULT ∙/
data Default
  = -- | /DEFAULT (∙)/
    Default'Expression Expression
  | -- | /DEFAULT ∙/
    Default'LiteralValue LiteralValue
  | -- | /DEFAULT ∙/
    Default'SignedNumber SignedNumber
  deriving stock (Eq, Generic, Show)

data GeneratedType
  = -- | /STORED/
    Stored
  | -- | /VIRTUAL/
    Virtual
  deriving stock (Eq, Generic, Show)

data Sign
  = -- | /-/
    Sign'HyphenMinus
  | -- | /+/
    Sign'PlusSign
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/signed-number.html
data SignedNumber
  = SignedNumber Sign Text
  | UnsignedNumber Text
  deriving stock (Eq, Generic, Show)
