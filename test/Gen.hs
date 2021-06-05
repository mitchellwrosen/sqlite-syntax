module Gen where

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Sqlite.Syntax
import Prelude

genAggregateFunctionCall :: Gen AggregateFunctionCall
genAggregateFunctionCall =
  AggregateFunctionCall
    <$> genFunctionCall genAggregateFunctionArguments
    <*> Gen.maybe genExpression

genAggregateFunctionArguments :: Gen (AggregateFunctionArguments Expression)
genAggregateFunctionArguments =
  Gen.choice
    [ pure AggregateFunctionArguments'None,
      pure AggregateFunctionArguments'Wildcard,
      AggregateFunctionArguments'Arguments <$> Gen.bool <*> Gen.nonEmpty (Range.linear 1 5) genExpression
    ]

genBindParameter :: Gen BindParameter
genBindParameter = undefined

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
    [ Expression'BindParameter <$> genBindParameter,
      Expression'Column <$> genSchemaQualified (genTableQualified genIdentifier),
      Expression'LiteralValue <$> genLiteralValue,
      Expression'Raise <$> genRaise
    ]
    [ Expression'AggregateFunctionCall <$> genAggregateFunctionCall,
      Expression'Case <$> genCaseExpression,
      Expression'Exists <$> genSelectStatement,
      Expression'FunctionCall <$> genFunctionCall genFunctionArguments,
      Expression'Glob <$> genExpression <*> genExpression <*> Gen.maybe genExpression,
      Expression'Like <$> genExpression <*> genExpression <*> Gen.maybe genExpression,
      Expression'Match <$> genExpression <*> genExpression <*> Gen.maybe genExpression,
      Expression'Regexp <$> genExpression <*> genExpression <*> Gen.maybe genExpression,
      Expression'Subquery <$> genSelectStatement,
      Expression'WindowFunctionCall <$> genWindowFunctionCall,
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
        (\table -> Expression'InTable (InTableExpression expression table)) <$> genSchemaQualified genIdentifier,
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
    <$> genSchemaQualified genIdentifier
    <*> gen

genGroupByClause :: Gen GroupByClause
genGroupByClause =
  GroupByClause
    <$> Gen.nonEmpty (Range.linear 1 5) genExpression
    <*> Gen.maybe genExpression

genLiteralValue :: Gen LiteralValue
genLiteralValue =
  Gen.choice
    [ pure LiteralValue'Null,
      LiteralValue'Boolean <$> Gen.bool,
      pure LiteralValue'CurrentDate,
      pure LiteralValue'CurrentTime,
      pure LiteralValue'CurrentTimestamp,
      LiteralValue'Number <$> Gen.element ["1", "1.0"],
      LiteralValue'Blob <$> Gen.element ["00", "01"],
      LiteralValue'String <$> Gen.element ["foo", "bar"]
    ]

-- TODO better generator
genIdentifier :: Gen Text
genIdentifier =
  Gen.element ["foo", "bar", "baz"]

genRaise :: Gen Raise
genRaise =
  Gen.element
    [ Raise'Ignore,
      Raise'Abort "error",
      Raise'Fail "error",
      Raise'Rollback "error"
    ]

genSchemaQualified :: Gen a -> Gen (SchemaQualified a)
genSchemaQualified gen =
  SchemaQualified <$> Gen.maybe genIdentifier <*> gen

genSelect :: Gen Select
genSelect =
  Select
    <$> Gen.bool
    <*> Gen.nonEmpty (Range.linear 1 5) (genSchemaQualified (genTableQualified genIdentifier))
    <*> Gen.maybe genTable
    <*> Gen.maybe genExpression
    <*> Gen.maybe genGroupByClause
    <*> Gen.maybe genWindowClause
  where
    genWindowClause :: Gen (NonEmpty (Aliased Identity WindowDefinition))
    genWindowClause = undefined

genSelectStatement :: Gen SelectStatement
genSelectStatement = undefined

genTable :: Gen Table
genTable = undefined

genTableQualified :: Gen a -> Gen (TableQualified a)
genTableQualified gen =
  TableQualified <$> Gen.maybe genIdentifier <*> gen

genWindowFunctionCall :: Gen WindowFunctionCall
genWindowFunctionCall = undefined
