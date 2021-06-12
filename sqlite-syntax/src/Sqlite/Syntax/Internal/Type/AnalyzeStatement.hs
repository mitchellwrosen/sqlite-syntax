module Sqlite.Syntax.Internal.Type.AnalyzeStatement
  ( AnalyzeStatement (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Namespaced (Namespaced)
import Prelude

-- | https://sqlite.org/lang_analyze.html
data AnalyzeStatement
  = AnalyzeStatement (Maybe (Namespaced Text Text))
  deriving stock (Eq, Generic, Show)
