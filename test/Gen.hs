module Gen where

import Data.Text (Text)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Sqlite.Syntax
import Prelude

genExpression :: Gen Expression
genExpression =
  undefined

-- TODO better generator
genIdentifier :: Gen Text
genIdentifier =
  Gen.element ["foo", "bar", "baz"]

genSchemaQualified :: Gen a -> Gen (SchemaQualified a)
genSchemaQualified gen =
  SchemaQualified <$> Gen.maybe genIdentifier <*> gen

genSelect :: Gen Select
genSelect =
  Select
    <$> Gen.bool
    <*> Gen.nonEmpty (Range.linear 1 5) (genSchemaQualified (genTableQualified genIdentifier))
    <*> Gen.maybe genFromClause
    <*> Gen.maybe genExpression
    <*> Gen.maybe genGroupByClause
    <*> Gen.maybe genWindowClause
  where
    genFromClause :: Gen FromClause
    genFromClause = undefined

    genGroupByClause :: Gen GroupByClause
    genGroupByClause = undefined

    genWindowClause :: Gen WindowClause
    genWindowClause = undefined

genTableQualified :: Gen a -> Gen (TableQualified a)
genTableQualified gen =
  TableQualified <$> Gen.maybe genIdentifier <*> gen
