{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sqlite.Syntax.Pretty
  (
  )
where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Prettyprinter
import Sqlite.Syntax
-- TODO remove
import Sqlite.Syntax.Parser
import Prelude hiding (Ordering)

commaSep :: [Doc a] -> [Doc a]
commaSep =
  punctuate comma

commaSepIndented :: [Doc a] -> Doc a
commaSepIndented =
  nest 2 . foldMap (softline <>) . commaSep

hardlines :: [Doc a] -> Doc a
hardlines =
  concatWith
    (\x y -> x <> hardline <> y)

parenthesized :: Doc a -> Doc a
parenthesized x =
  lparen <> nest 2 (softline' <> x) <> softline' <> rparen

--

prettyMaybeAliased :: Pretty a => Aliased Maybe a -> Doc b
prettyMaybeAliased = \case
  Aliased x Nothing -> pretty x
  Aliased x (Just y) -> hsep [pretty x, "AS", pretty y]

--

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
    Expression'And {} -> undefined
    Expression'Between {} -> undefined
    Expression'BitwiseAnd {} -> undefined
    Expression'BitwiseNegate {} -> undefined
    Expression'BitwiseOr {} -> undefined
    Expression'Case {} -> undefined
    Expression'Cast {} -> undefined
    Expression'Collate {} -> undefined
    Expression'Column x -> pretty x
    Expression'Concatenate {} -> undefined
    Expression'Divide {} -> undefined
    Expression'Equals x y -> binop "=" x y
    Expression'Exists {} -> undefined
    Expression'FunctionCall {} -> undefined
    Expression'Glob {} -> undefined
    Expression'GreaterThan {} -> undefined
    Expression'GreaterThanOrEquals {} -> undefined
    Expression'InFunction {} -> undefined
    Expression'InSubquery {} -> undefined
    Expression'InTable {} -> undefined
    Expression'InValues {} -> undefined
    Expression'Is {} -> undefined
    Expression'LessThan x y -> binop "<" x y
    Expression'LessThanOrEquals {} -> undefined
    Expression'Like {} -> undefined
    Expression'LiteralValue x -> pretty x
    Expression'Match {} -> undefined
    Expression'Minus {} -> undefined
    Expression'Modulo {} -> undefined
    Expression'Multiply {} -> undefined
    Expression'Negate {} -> undefined
    Expression'Not {} -> undefined
    Expression'NotEquals {} -> undefined
    Expression'Or {} -> undefined
    Expression'Parameter {} -> undefined
    Expression'Plus {} -> undefined
    Expression'Raise {} -> undefined
    Expression'Regexp {} -> undefined
    Expression'RowValue {} -> undefined
    Expression'ShiftLeft {} -> undefined
    Expression'ShiftRight {} -> undefined
    Expression'Subquery {} -> undefined
    where
      binop s x y =
        parenthesized (hsep [pretty x, s, pretty y])

instance Pretty GroupBy where
  pretty GroupBy {groupBy, having} =
    group $
      "GROUP BY"
        <> commaSepIndented (map pretty (toList groupBy))
        <> maybe mempty (\e -> line <> "HAVING" <+> pretty e) having

instance Pretty JoinConstraint where
  pretty = \case
    On x -> "ON" <+> pretty x
    Using _ -> undefined

instance Pretty Limit where
  pretty = undefined

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
    String _ -> undefined

instance Pretty Ordering where
  pretty = \case
    Ascending -> "ASC"
    Descending -> "DESC"

instance Pretty OrderingTerm where
  pretty OrderingTerm {expression, collation, ordering, nullsPlacement} =
    case collation of
      Nothing -> hsep [pretty expression, pretty ordering, pretty nullsPlacement]
      Just _ -> undefined

instance (Pretty a, Pretty b) => Pretty (Namespaced a b) where
  pretty (Namespaced x y) =
    maybe mempty ((<> dot) . pretty) x <> pretty y

instance Pretty QualifiedTableName where
  pretty QualifiedTableName {name, indexedBy} =
    case indexedBy of
      Nothing -> prettyMaybeAliased name
      Just _ -> undefined

instance Pretty NullsPlacement where
  pretty = \case
    NullsFirst -> "NULLS FIRST"
    NullsLast -> "NULLS LAST"

instance Pretty ResultColumn where
  pretty = \case
    ResultColumn'Expression x -> prettyMaybeAliased x
    ResultColumn'Wildcard (Namespaced mx ()) -> maybe "*" (\x -> pretty x <> dot <> "*") mx

instance Pretty Returning where
  pretty = undefined

instance Pretty Select where
  pretty Select {distinct, columns, from, where_, groupBy, window} =
    hardlines
      ( ( "SELECT"
            <> (if distinct then space <> "DISTINCT" else mempty)
            <> commaSepIndented (map pretty (toList columns))
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
        commaSepIndented
          . map (\(Aliased x (Identity y)) -> hsep [pretty y, "AS", pretty x])
          . toList

instance Pretty SelectCore where
  pretty = \case
    SelectCore'Select x -> pretty x
    SelectCore'Values _ -> undefined

instance Pretty SelectStatement where
  pretty SelectStatement {commonTableExpressions, select, orderBy, limit} =
    case commonTableExpressions of
      Nothing ->
        sep
          ( pretty select :
            catMaybes
              [ (("ORDER BY" <>) . commaSepIndented . map pretty . toList) <$> orderBy,
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
    Statement'Insert {} -> undefined
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
    TransactionType'Deferred -> "DEFERRED"
    TransactionType'Exclusive -> "EXCLUSIVE"
    TransactionType'Immediate -> "IMMEDIATE"

instance Pretty Window where
  pretty = undefined
