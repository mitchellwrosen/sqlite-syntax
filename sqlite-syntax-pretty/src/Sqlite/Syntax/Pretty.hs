{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sqlite.Syntax.Pretty
  (
  )
where

import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Prettyprinter hiding (list)
import Sqlite.Syntax
import Prelude hiding (Ordering, filter)

hardlines :: [Doc a] -> Doc a
hardlines =
  concatWith
    (\x y -> x <> hardline <> y)

list :: [Doc a] -> Doc a
list =
  sep . punctuate comma

list1 :: NonEmpty (Doc a) -> Doc a
list1 =
  list . NonEmpty.toList

parenthesized :: Doc a -> Doc a
parenthesized x =
  lparen <> nest 2 (softline' <> x) <> softline' <> rparen

--

prettyAsAlias :: Pretty a => Aliased Maybe a -> Doc b
prettyAsAlias = \case
  Aliased x Nothing -> pretty x
  Aliased x (Just y) -> hsep [pretty x, "AS", pretty y]

--

instance Pretty CaseExpression where
  pretty CaseExpression {base, cases, else_} =
    hardlines
      ( "CASE" <> maybe mempty (group . pretty) base :
        indent 2 (hardlines (map prettyCase (NonEmpty.toList cases))) :
        indent 2 ("ELSE" <+> pretty else_) :
        ["END"]
      )
    where
      prettyCase :: (Expression, Expression) -> Doc a
      prettyCase (x, y) =
        "WHEN" <+> group (pretty x) <+> "THEN" <+> group (pretty y)

instance Pretty CastExpression where
  pretty CastExpression {expression, type_} =
    "CAST" <+> parenthesized (pretty expression <+> "AS" <+> pretty type_)

instance Pretty CommonTableExpressions where
  pretty = undefined

instance Pretty CompoundSelect where
  pretty = \case
    CompoundSelect x -> pretty x
    Except x y -> compound "EXCEPT" x y
    Intersect x y -> compound "INTERSECT" x y
    Union x y -> compound "UNION" x y
    UnionAll x y -> compound "UNION ALL" x y
    where
      compound s x y =
        hardlines [pretty x, s, pretty y]

instance Pretty DeleteStatement where
  pretty DeleteStatement {commonTableExpressions, table, where_, returning} =
    maybe mempty ((<> hardline) . pretty) commonTableExpressions
      <> "DELETE FROM" <+> pretty table
      <> maybe mempty ((hardline <>) . ("WHERE" <+>) . pretty) where_
      <> maybe mempty ((hardline <>) . pretty) returning

-- TODO fewer parens
instance Pretty Expression where
  pretty = \case
    Expression'AggregateDistinctFunctionCall {} -> undefined
    Expression'And x y -> binop "AND" x y
    Expression'Between x y z ->
      parenthesized
        ( hsep
            [ parenthesized (pretty x),
              "BETWEEN",
              parenthesized (pretty y),
              "AND",
              parenthesized (pretty z)
            ]
        )
    Expression'BitwiseAnd x y -> binop "&" x y
    Expression'BitwiseNegate x -> unop "~" x
    Expression'BitwiseOr x y -> binop "|" x y
    Expression'Case x -> pretty x
    Expression'Cast x -> pretty x
    Expression'Collate {} -> undefined
    Expression'Column x -> pretty x
    Expression'Concatenate x y -> binop "||" x y
    Expression'Divide x y -> binop "/" x y
    Expression'Equals x y -> binop "=" x y
    Expression'Exists x -> "EXISTS" <+> pretty x
    Expression'FunctionCall x -> pretty x
    Expression'Glob {} -> undefined
    Expression'GreaterThan x y -> binop ">" x y
    Expression'GreaterThanOrEquals x y -> binop ">=" x y
    Expression'InFunction {} -> undefined
    Expression'InSubquery {} -> undefined
    Expression'InTable {} -> undefined
    Expression'InValues x -> pretty x
    Expression'Is x y -> binop "IS" x y
    Expression'LessThan x y -> binop "<" x y
    Expression'LessThanOrEquals x y -> binop "<=" x y
    Expression'Like {} -> undefined
    Expression'LiteralValue x -> pretty x
    Expression'Match {} -> undefined
    Expression'Minus x y -> binop "-" x y
    Expression'Modulo x y -> binop "%" x y
    Expression'Multiply x y -> binop "*" x y
    Expression'Negate x -> unop "-" x
    Expression'Not x -> unop "NOT" x
    Expression'NotEquals x y -> binop "<>" x y
    Expression'Or x y -> binop "OR" x y
    Expression'Parameter {} -> undefined
    Expression'Plus x y -> binop "+" x y
    Expression'Raise {} -> undefined
    Expression'Regexp {} -> undefined
    Expression'RowValue {} -> undefined
    Expression'ShiftLeft x y -> binop "<<" x y
    Expression'ShiftRight x y -> binop ">>" x y
    Expression'Subquery x -> parenthesized (pretty x)
    where
      unop s x =
        parenthesized (s <+> parenthesized (pretty x))

      binop s x y =
        parenthesized (hsep [parenthesized (pretty x), s, parenthesized (pretty y)])

instance Pretty (FunctionArguments Expression) where
  pretty = \case
    FunctionArguments'Arguments expressions -> list (pretty <$> expressions)
    FunctionArguments'Wildcard -> "*"

instance Pretty (FunctionCall FunctionArguments) where
  pretty FunctionCall {name, arguments} =
    pretty name <> parenthesized (pretty arguments)

instance Pretty FunctionCallExpression where
  pretty FunctionCallExpression {call, filter, over} =
    pretty call <> maybe mempty undefined filter <> maybe mempty undefined over

instance Pretty GroupBy where
  pretty GroupBy {groupBy, having} =
    group $
      "GROUP BY"
        <> nest 2 (softline <> list1 (pretty <$> groupBy))
        <> maybe mempty (\e -> line <> "HAVING" <+> pretty e) having

instance Pretty InsertStatement where
  pretty InsertStatement {commonTableExpressions, onConflict, table, columns, insert, returning} =
    case (commonTableExpressions, returning) of
      (Nothing, Nothing) ->
        hardlines
          [ mconcat
              [ case onConflict of
                  Abort -> "INSERT"
                  Fail -> "INSERT OR FAIL"
                  Ignore -> "INSERT OR IGNORE"
                  Replace -> "REPLACE"
                  Rollback -> "INSERT OR ROLLBACK",
                " INTO ",
                prettyAsAlias table,
                maybe mempty (\cs -> space <> parenthesized (list1 (pretty <$> cs))) columns
              ],
            case insert of
              InsertDefaultValues -> "DEFAULT VALUES"
              InsertSelect select Nothing -> pretty select
              InsertSelect _ (Just _) -> undefined
          ]
      _ -> undefined

instance Pretty InValuesExpression where
  pretty InValuesExpression {expression, values} =
    pretty expression <+> "IN" <+> parenthesized (list (pretty <$> values))

instance Pretty JoinConstraint where
  pretty = \case
    On x -> "ON" <+> pretty x
    Using _ -> undefined

instance Pretty Limit where
  pretty Limit {limit, offset} =
    "LIMIT" <+> pretty limit <> maybe mempty (\o -> hardline <> "OFFSET" <+> pretty o) offset

instance Pretty LiteralValue where
  pretty = \case
    Blob _ -> undefined
    Boolean True -> "TRUE"
    Boolean False -> "FALSE"
    CurrentDate -> "CURRENT_DATE"
    CurrentTime -> "CURRENT_TIME"
    CurrentTimestamp -> "CURRENT_TIMESTAMP"
    Null -> "NULL"
    Number x -> pretty x
    String x -> pretty ("'" <> Text.replace "'" "''" x <> "'")

instance Pretty Ordering where
  pretty = \case
    Ascending -> "ASC"
    Descending -> "DESC"

instance Pretty OrderingTerm where
  pretty OrderingTerm {expression, collation, ordering, nullsPlacement} =
    case collation of
      Nothing -> hsep [pretty expression, pretty ordering, pretty nullsPlacement]
      Just _ -> undefined

instance Pretty a => Pretty (Namespaced a Text) where
  pretty (Namespaced x y) =
    maybe mempty ((<> dot) . pretty) x <> pretty y

instance Pretty QualifiedTableName where
  pretty QualifiedTableName {name, indexedBy} =
    case indexedBy of
      Nothing -> prettyAsAlias name
      Just _ -> undefined

instance Pretty NullsPlacement where
  pretty = \case
    NullsFirst -> "NULLS FIRST"
    NullsLast -> "NULLS LAST"

instance Pretty ResultColumn where
  pretty = \case
    ResultColumn'Expression x -> prettyAsAlias x
    ResultColumn'Wildcard (Namespaced mx ()) -> maybe "*" (\x -> pretty x <> dot <> "*") mx

instance Pretty Returning where
  pretty = undefined

instance Pretty Select where
  pretty Select {distinct, columns, from, where_, groupBy, window} =
    hardlines
      ( ( "SELECT"
            <> (if distinct then space <> "DISTINCT" else mempty)
            <> nest 2 (softline <> list1 (pretty <$> columns))
        ) :
        catMaybes
          [ (("FROM" <+>) . pretty) <$> from,
            (("WHERE" <+>) . pretty) <$> where_,
            pretty <$> groupBy,
            (("WINDOW" <+>) . prettyWindow) <$> window
          ]
      )
    where
      prettyWindow :: NonEmpty (Aliased Identity Window) -> Doc a
      prettyWindow =
        nest 2
          . (softline <>)
          . list1
          . fmap (\(Aliased x (Identity y)) -> hsep [pretty y, "AS", pretty x])

instance Pretty SelectCore where
  pretty = \case
    SelectCore'Select x -> pretty x
    SelectCore'Values x -> pretty x

instance Pretty SelectStatement where
  pretty SelectStatement {commonTableExpressions, select, orderBy, limit} =
    case commonTableExpressions of
      Nothing ->
        sep
          ( pretty select :
            catMaybes
              [ (("ORDER BY" <>) . nest 2 . (softline <>) . list1 . fmap pretty) <$> orderBy,
                pretty <$> limit
              ]
          )
      Just _ -> undefined

instance Pretty Statement where
  pretty = \case
    Statement'Select x -> pretty x
    Statement'AlterTable {} -> undefined
    Statement'Analyze {} -> undefined
    Statement'Attach {} -> undefined
    Statement'Begin x -> hsep ["BEGIN", pretty x, "TRANSACTION"]
    Statement'Commit {} -> undefined
    Statement'CreateIndex {} -> undefined
    Statement'CreateTable {} -> undefined
    Statement'CreateTrigger {} -> undefined
    Statement'CreateView {} -> undefined
    Statement'CreateVirtualTable {} -> undefined
    Statement'Delete x -> pretty x
    Statement'Detach {} -> undefined
    Statement'DropIndex {} -> undefined
    Statement'DropTable {} -> undefined
    Statement'DropTrigger {} -> undefined
    Statement'DropView {} -> undefined
    Statement'Insert x -> pretty x
    Statement'Pragma {} -> undefined
    Statement'Reindex {} -> undefined
    Statement'Release {} -> undefined
    Statement'Rollback {} -> undefined
    Statement'Savepoint {} -> undefined
    Statement'Update {} -> undefined
    Statement'Vacuum {} -> undefined

instance Pretty Table where
  pretty = \case
    Table x -> pretty x
    Table'CrossJoin {} -> undefined
    Table'Function {} -> undefined
    Table'InnerJoin x y z ->
      group do
        pretty x
          <> line
          <> "INNER JOIN"
          <> line
          <> pretty0 y
          <> maybe mempty ((line <>) . pretty) z
    Table'LeftOuterJoin {} -> undefined
    Table'NaturalCrossJoin {} -> undefined
    Table'NaturalInnerJoin {} -> undefined
    Table'NaturalLeftOuterJoin {} -> undefined
    Table'Subquery (Aliased x Nothing) -> parenthesized (pretty x)
    Table'Subquery (Aliased x (Just y)) -> hsep [parenthesized (pretty x), "AS", pretty y]
    where
      pretty0 table =
        case table of
          Table {} -> ptable
          Table'CrossJoin {} -> parenthesized ptable
          Table'Function {} -> undefined
          Table'InnerJoin {} -> parenthesized ptable
          Table'LeftOuterJoin {} -> parenthesized ptable
          Table'NaturalCrossJoin {} -> parenthesized ptable
          Table'NaturalInnerJoin {} -> parenthesized ptable
          Table'NaturalLeftOuterJoin {} -> parenthesized ptable
          Table'Subquery {} -> undefined
        where
          ptable =
            pretty table

instance Pretty TransactionType where
  pretty = \case
    Deferred -> "DEFERRED"
    Exclusive -> "EXCLUSIVE"
    Immediate -> "IMMEDIATE"

instance Pretty Values where
  pretty (Values xs) =
    "VALUES" <> nest 2 (softline <> list1 (parenthesized . list1 . fmap pretty <$> xs))

instance Pretty Window where
  pretty = undefined
