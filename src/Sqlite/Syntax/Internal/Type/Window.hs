module Sqlite.Syntax.Internal.Type.Window where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Functor.Identity (Identity)
import {-# SOURCE #-} Sqlite.Syntax.Internal.Type.Expression (Expression)
import Sqlite.Syntax.Internal.Type.OrderingTerm (OrderingTerm)
import Prelude

-- | https://sqlite.org/syntax/frame-spec.html
data Frame = Frame
  { type_ :: FrameType,
    startingBoundary :: FrameBoundary Identity Maybe,
    endingBoundary :: FrameBoundary Maybe Identity,
    exclude :: FrameExclude
  }
  deriving stock (Eq, Generic, Show)

data FrameBoundary f g
  = FrameBoundary'CurrentRow
  | FrameBoundary'Following (f Expression)
  | FrameBoundary'Preceding (g Expression)
  deriving stock (Generic)

deriving instance (Eq (f Expression), Eq (g Expression)) => Eq (FrameBoundary f g)
deriving instance (Show (f Expression), Show (g Expression)) => Show (FrameBoundary f g)

data FrameExclude
  = FrameExclude'CurrentRow
  | FrameExclude'Group
  | FrameExclude'NoOthers
  | FrameExclude'Ties
  deriving stock (Eq, Generic, Show)

data FrameType
  = FrameType'Groups
  | FrameType'Range
  | FrameType'Rows
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/window-defn.html
data Window = Window
  { base :: Maybe Text,
    partitionBy :: Maybe (NonEmpty Expression),
    orderBy :: Maybe (NonEmpty OrderingTerm),
    frame :: Frame
  }
  deriving stock (Eq, Generic, Show)
