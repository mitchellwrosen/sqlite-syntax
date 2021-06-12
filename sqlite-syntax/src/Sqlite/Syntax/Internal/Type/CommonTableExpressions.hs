module Sqlite.Syntax.Internal.Type.CommonTableExpressions
  ( CommonTableExpression (..),
    CommonTableExpressions (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.SelectStatement (SelectStatement)
import Prelude

-- | https://sqlite.org/syntax/common-table-expression.html
--
-- @
-- __table__ [(__column__+)] /AS/ [[/NOT/] /MATERIALIZED/] (__select-statement__)
-- @
data CommonTableExpression = CommonTableExpression
  { table :: Text, -- TODO could this be namespaced?
    columns :: Maybe (NonEmpty Text),
    materialized :: Maybe Bool,
    select :: SelectStatement
  }
  deriving stock (Eq, Generic, Show)

-- |
-- @
-- /WITH/ [/RECURSIVE/] __common-table-expression__+
-- @
--
-- TODO move
data CommonTableExpressions = CommonTableExpressions
  { recursive :: Bool,
    tables :: NonEmpty CommonTableExpression
  }
  deriving stock (Eq, Generic, Show)
