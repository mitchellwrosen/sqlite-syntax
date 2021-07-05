module Gen where

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Sqlite.Syntax
import Prelude

genAggregateDistinctFunctionCallExpression :: Gen AggregateDistinctFunctionCallExpression
genAggregateDistinctFunctionCallExpression =
  AggregateDistinctFunctionCallExpression
    <$> genFunctionCall (Identity <$> genExpression)
    <*> Gen.maybe genExpression

genCaseExpression :: Gen CaseExpression
genCaseExpression =
  CaseExpression
    <$> Gen.maybe genExpression
    <*> Gen.nonEmpty (Range.linear 1 5) ((,) <$> genExpression <*> genExpression)
    <*> genExpression

genExpression :: Gen Expression
genExpression =
  Gen.recursive
    Gen.choice
    [ Expression'Column <$> genNamespaced (genNamespaced genIdentifier genIdentifier) (genIdentifier),
      Expression'LiteralValue <$> genLiteralValue,
      Expression'Parameter <$> genParameter,
      Expression'Raise <$> genRaise
    ]
    [ Expression'AggregateDistinctFunctionCall <$> genAggregateDistinctFunctionCallExpression,
      Expression'Case <$> genCaseExpression,
      Expression'Exists <$> genSelectStatement,
      Expression'FunctionCall <$> genFunctionCallExpression,
      Expression'Glob <$> genExpression <*> genExpression <*> Gen.maybe genExpression,
      Expression'Like <$> genExpression <*> genExpression <*> Gen.maybe genExpression,
      Expression'Match <$> genExpression <*> genExpression <*> Gen.maybe genExpression,
      Expression'Regexp <$> genExpression <*> genExpression <*> Gen.maybe genExpression,
      Expression'Subquery <$> genSelectStatement,
      Gen.subterm genExpression Expression'BitwiseNegate,
      Gen.subtermM genExpression \expression ->
        (\type_ -> Expression'Cast (CastExpression expression type_)) <$> genIdentifier,
      Gen.subtermM genExpression \expression ->
        (\name -> Expression'Collate (CollateExpression expression name)) <$> genIdentifier,
      Gen.subtermM genExpression \expression ->
        (\function -> Expression'InFunction (InFunctionExpression expression function))
          <$> genFunctionCall (Gen.list (Range.linear 0 5) genExpression),
      Gen.subtermM genExpression \expression ->
        (\subquery -> Expression'InSubquery (InSubqueryExpression expression subquery)) <$> genSelectStatement,
      Gen.subtermM genExpression \expression ->
        (\table -> Expression'InTable (InTableExpression expression table))
          <$> genNamespaced genIdentifier genIdentifier,
      Gen.subtermM genExpression \expression ->
        (\values -> Expression'InValues (InValuesExpression expression values))
          <$> Gen.list (Range.linear 0 5) genExpression,
      Gen.subterm genExpression Expression'Negate,
      Gen.subterm genExpression Expression'Not,
      Gen.subterm2 genExpression genExpression Expression'And,
      Gen.subterm2 genExpression genExpression Expression'BitwiseAnd,
      Gen.subterm2 genExpression genExpression Expression'Concatenate,
      Gen.subterm2 genExpression genExpression Expression'Divide,
      Gen.subterm2 genExpression genExpression Expression'Equals,
      Gen.subterm2 genExpression genExpression Expression'GreaterThan,
      Gen.subterm2 genExpression genExpression Expression'GreaterThanOrEquals,
      Gen.subterm2 genExpression genExpression Expression'Is,
      Gen.subterm2 genExpression genExpression Expression'LessThan,
      Gen.subterm2 genExpression genExpression Expression'LessThanOrEquals,
      Gen.subterm2 genExpression genExpression Expression'Minus,
      Gen.subterm2 genExpression genExpression Expression'Modulo,
      Gen.subterm2 genExpression genExpression Expression'Multiply,
      Gen.subterm2 genExpression genExpression Expression'NotEquals,
      Gen.subterm2 genExpression genExpression Expression'Or,
      Gen.subterm2 genExpression genExpression Expression'Plus,
      Gen.subterm2 genExpression genExpression Expression'ShiftLeft,
      Gen.subterm2 genExpression genExpression Expression'ShiftRight,
      Gen.subtermM2 genExpression genExpression \expression1 expression2 ->
        (\expressions -> Expression'RowValue (RowValue expression1 expression2 expressions))
          <$> Gen.list (Range.linear 0 5) genExpression,
      Gen.subterm3 genExpression genExpression genExpression Expression'Between
    ]

genFunctionArguments :: Gen (FunctionArguments Expression)
genFunctionArguments =
  Gen.choice
    [ FunctionArguments'Arguments <$> Gen.list (Range.linear 0 5) genExpression,
      pure FunctionArguments'Wildcard
    ]

genFunctionCall :: Gen (f Expression) -> Gen (FunctionCall f)
genFunctionCall gen =
  FunctionCall
    <$> genNamespaced genIdentifier genIdentifier
    <*> gen

genFunctionCallExpression :: Gen FunctionCallExpression
genFunctionCallExpression =
  FunctionCallExpression
    <$> genFunctionCall genFunctionArguments
    <*> Gen.maybe genExpression
    <*> Gen.maybe genOver

genGroupBy :: Gen GroupBy
genGroupBy =
  GroupBy
    <$> Gen.nonEmpty (Range.linear 1 5) genExpression
    <*> Gen.maybe genExpression

genLiteralValue :: Gen LiteralValue
genLiteralValue =
  Gen.choice
    [ pure Null,
      Boolean <$> Gen.bool,
      pure CurrentDate,
      pure CurrentTime,
      pure CurrentTimestamp,
      Number <$> Gen.element ["1", "1.0"],
      Blob <$> Gen.element ["00", "01"],
      String <$> Gen.element ["foo", "bar"]
    ]

-- TODO better generator
genIdentifier :: Gen Text
genIdentifier =
  Gen.element ["foo", "bar", "baz"]

genNamespaced :: Gen a -> Gen b -> Gen (Namespaced a b)
genNamespaced g1 g2 =
  Namespaced <$> Gen.maybe g1 <*> g2

genOver :: Gen Over
genOver = undefined

genParameter :: Gen Parameter
genParameter = undefined

genRaise :: Gen Raise
genRaise =
  Gen.element
    [ Raise'Ignore,
      Raise'Abort "error",
      Raise'Fail "error",
      Raise'Rollback "error"
    ]

genResultColumn :: Gen ResultColumn
genResultColumn =
  undefined

genSelect :: Gen Select
genSelect =
  Select
    <$> Gen.bool
    <*> Gen.nonEmpty (Range.linear 1 5) genResultColumn
    <*> Gen.maybe genTable
    <*> Gen.maybe genExpression
    <*> Gen.maybe genGroupBy
    <*> Gen.maybe genWindow
  where
    genWindow :: Gen (NonEmpty (Aliased Identity Window))
    genWindow = undefined

genSelectStatement :: Gen SelectStatement
genSelectStatement = undefined

genTable :: Gen Table
genTable = undefined
