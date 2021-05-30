module Sqlite.Syntax.Internal.Type.FunctionCall
  ( FunctionCall (..),
  )
where

import Data.Text (Text)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.SchemaQualified (SchemaQualified)

data FunctionCall f = FunctionCall
  { name :: SchemaQualified Text,
    arguments :: f Expression
  }
