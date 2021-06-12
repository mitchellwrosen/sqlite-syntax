module Sqlite.Syntax.Internal.Type.TransactionType
  ( TransactionType (..),
  )
where

import GHC.Generics (Generic)
import Prelude

data TransactionType
  = Deferred
  | Exclusive
  | Immediate
  deriving stock (Eq, Generic, Show)
