module Sqlite.Syntax.Internal.Type.Columns
  ( Columns,
  )
where

import Data.Text (Text)
import Sqlite.Syntax.Internal.Type.Namespaced (Namespaced)

type Columns f =
  Namespaced (Namespaced Text Text) (f Text)
