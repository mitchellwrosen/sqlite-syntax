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
import Prelude hiding (Ordering)

commaSep :: [Doc a] -> [Doc a]
commaSep =
  punctuate comma

commaSepIndented :: [Doc a] -> Doc a
commaSepIndented =
  nest 2 . foldMap (line <>) . commaSep

instance Pretty CompoundSelect where
  pretty = \case
    CompoundSelect x -> pretty x
    Except x y -> compound "EXCEPT" x y
    Intersect x y -> compound "INTERSECT" x y
    Union x y -> compound "UNION" x y
    UnionAll x y -> compound "UNION ALL" x y
    where
      compound s x y =
        sep [pretty x, s, pretty y]

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
    Expression'Equals {} -> undefined
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
    Expression'LessThan {} -> undefined
    Expression'LessThanOrEquals {} -> undefined
    Expression'Like {} -> undefined
    Expression'LiteralValue {} -> undefined
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

instance Pretty GroupBy where
  pretty GroupBy {groupBy, having} =
    group $
      "GROUP BY"
        <> commaSepIndented (map pretty (toList groupBy))
        <> maybe mempty (\e -> line <> "HAVING" <+> pretty e) having

instance Pretty Limit where
  pretty = undefined

instance Pretty Ordering where
  pretty = \case
    Ascending -> "ASC"
    Descending -> "DESC"

instance Pretty OrderingTerm where
  pretty = undefined

instance (Pretty a, Pretty b) => Pretty (Namespaced a b) where
  pretty (Namespaced x y) =
    maybe mempty ((<> dot) . pretty) x <> pretty y

instance Pretty NullsPlacement where
  pretty = \case
    NullsFirst -> "NULLS FIRST"
    NullsLast -> "NULLS LAST"

instance Pretty ResultColumn where
  pretty = \case
    ResultColumn'Expression (Aliased x y) -> pretty x <> maybe mempty ((space <>) . ("AS" <+>) . pretty) y
    ResultColumn'Wildcard (Namespaced mx ()) -> maybe "*" (\x -> pretty x <> dot <> "*") mx

instance Pretty Select where
  pretty Select {distinct, columns, from, where_, groupBy, window} =
    sep
      ( ("SELECT" <+> (if distinct then "DISTINCT" else "ALL") <> commaSepIndented (map pretty (toList columns))) :
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
    Statement'Begin {} -> undefined
    Statement'Commit {} -> undefined
    Statement'CreateIndex {} -> undefined
    Statement'CreateTable {} -> undefined
    Statement'CreateTrigger {} -> undefined
    Statement'CreateView {} -> undefined
    Statement'CreateVirtualTable {} -> undefined
    Statement'Delete {} -> undefined
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
    Table {} -> undefined
    Table'CrossJoin {} -> undefined
    Table'Function {} -> undefined
    Table'InnerJoin {} -> undefined
    Table'LeftOuterJoin {} -> undefined
    Table'NaturalCrossJoin {} -> undefined
    Table'NaturalInnerJoin {} -> undefined
    Table'NaturalLeftOuterJoin {} -> undefined
    Table'Subquery {} -> undefined

instance Pretty Window where
  pretty = undefined
