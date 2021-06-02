module Sqlite.Syntax.Internal.Type.FunctionCall
  ( FunctionCall (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.SchemaQualified (SchemaQualified)
import Prelude

data FunctionCall f = FunctionCall
  { name :: SchemaQualified Text,
    arguments :: f Expression
  }
  deriving stock (Generic)

deriving instance Eq (f Expression) => Eq (FunctionCall f)

deriving instance Show (f Expression) => Show (FunctionCall f)
