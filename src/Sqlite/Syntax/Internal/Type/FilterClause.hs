module Sqlite.Syntax.Internal.Type.FilterClause
  ( FilterClause (..),
  )
where

import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression (Expression)

-- | https://sqlite.org/syntax/filter-clause.html
newtype FilterClause
  = FilterClause Expression
