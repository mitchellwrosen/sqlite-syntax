module Sqlite.Syntax.Internal.Type.Values
  ( Values (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression (Expression)
import Prelude

newtype Values
  = Values (NonEmpty (NonEmpty Expression))
  deriving stock (Eq, Generic, Show)
