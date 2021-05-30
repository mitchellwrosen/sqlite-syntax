module Sqlite.Syntax.Parser where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Control.Applicative.Combinators.NonEmpty (some)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.FilterClause
import Sqlite.Syntax.Internal.Type.FunctionCall
import Sqlite.Syntax.Internal.Type.LiteralValue
import Sqlite.Syntax.Internal.Type.SchemaQualified
import Sqlite.Syntax.Internal.Type.SelectStatement
import Sqlite.Syntax.Internal.Type.TableQualified
import qualified Sqlite.Syntax.Parser.Token as Token
import Sqlite.Syntax.Token (Token)
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, not, null)

-- TODO simplify some things with defaults (e.g. missing distinct/all == all)
-- TODO derive show/eq/generic
-- TODO flatten some things? e.g. UnaryOperator -> flatten into multiple Expression
-- TODO move signed-number to lexer probably
-- TODO FunctionName, etc. -> Identifier
-- TODO name things *Parser

type Parser r =
  Token.Parser r

commaSep0 :: Parser r a -> Parser r [a]
commaSep0 p =
  choice
    [ NonEmpty.toList <$> commaSep1 p,
      pure []
    ]

commaSep1 :: Parser r a -> Parser r (NonEmpty a)
commaSep1 p =
  (:|) <$> p <*> many (Token.comma *> p)

parens :: Parser r a -> Parser r a
parens p =
  Token.leftParenthesis *> p <* Token.rightParenthesis

perhaps :: Parser r a -> Parser r Bool
perhaps p =
  choice [True <$ p, pure False]

--

data Statement
  = Statement'AlterTable AlterTableStatement
  | Statement'Analyze AnalyzeStatement
  | Statement'Attach AttachStatement
  | Statement'Begin BeginStatement
  | Statement'Commit
  | Statement'CreateIndex CreateIndexStatement
  | Statement'CreateTable CreateTableStatement
  | Statement'CreateTrigger TODO
  | Statement'CreateView TODO
  | Statement'CreateVirtualTable TODO
  | Statement'Delete TODO
  | Statement'Detach TODO
  | Statement'DropIndex TODO
  | Statement'DropTable TODO
  | Statement'DropTrigger TODO
  | Statement'DropView TODO
  | Statement'Insert TODO
  | Statement'Pragma TODO
  | Statement'Reindex TODO
  | Statement'Release TODO
  | Statement'Rollback RollbackStatement
  | Statement'Savepoint TODO
  | Statement'Select TODO
  | Statement'Update TODO
  | Statement'Vacuum TODO

data TODO

statement :: Earley.Grammar r (Parser r Statement)
statement = mdo
  columnDefinition <- Earley.rule (makeColumnDefinition columnConstraint)
  expression <- makeExpression selectStatement
  indexedColumn <- Earley.rule (makeIndexedColumn expression)
  selectStatement <- Earley.rule (makeSelectStatement expression)

  let alterTableStatement = makeAlterTableStatement columnDefinition
      attachStatement = makeAttachStatement expression
      columnConstraint = makeColumnConstraint columnConstraintType
      columnConstraintType = makeColumnConstraintType expression
      createIndexStatement = makeCreateIndexStatement expression indexedColumn
      createTableStatement = makeCreateTableStatement columnDefinition expression indexedColumn selectStatement

  pure do
    choice
      [ Statement'AlterTable <$> alterTableStatement,
        Statement'Analyze <$> analyzeStatement,
        Statement'Attach <$> attachStatement,
        Statement'Begin <$> beginStatement,
        Statement'Commit <$ commitStatement,
        Statement'CreateIndex <$> createIndexStatement,
        Statement'CreateTable <$> createTableStatement,
        Statement'Rollback <$> rollbackStatement
      ]

--

-- | https://sqlite.org/lang_altertable.html
data AlterTableStatement = AlterTableStatement
  { table :: SchemaQualified Text,
    alteration :: TableAlteration
  }

makeAlterTableStatement :: Parser r ColumnDefinition -> Parser r AlterTableStatement
makeAlterTableStatement columnDefinition =
  AlterTableStatement
    <$> (Token.alter *> Token.table *> schemaQualified Token.identifier)
    <*> choice
      [ TableAlteration'AddColumn <$> columnDefinition,
        TableAlteration'DropColumn <$> Token.identifier,
        TableAlteration'Rename <$> (Token.rename *> Token.to *> Token.identifier),
        TableAlteration'RenameColumn
          <$> (Token.rename *> optional Token.column *> Token.identifier)
          <*> (Token.to *> Token.identifier)
      ]

-- | https://sqlite.org/lang_analyze.html
data AnalyzeStatement
  = AnalyzeStatement (Maybe (SchemaQualified IndexOrTableName))

analyzeStatement :: Parser r AnalyzeStatement
analyzeStatement =
  AnalyzeStatement
    <$> (Token.analyze *> optional (schemaQualified indexOrTableName))

-- | https://sqlite.org/syntax/attach-stmt.html
data AttachStatement = AttachStatement
  { database :: Expression,
    schema :: Text
  }

makeAttachStatement :: Parser r Expression -> Parser r AttachStatement
makeAttachStatement expression =
  AttachStatement
    <$> (Token.attach *> optional Token.database *> expression)
    <*> (Token.as *> Token.identifier)

-- | https://sqlite.org/syntax/begin-stmt.html
newtype BeginStatement
  = BeginStatement (Maybe TransactionType)

beginStatement :: Parser r BeginStatement
beginStatement =
  BeginStatement
    <$> (Token.begin *> optional transactionType <* optional Token.transaction)

bindParameter :: Parser r BindParameter
bindParameter = undefined

makeCastExpression :: Parser r Expression -> Parser r CastExpression
makeCastExpression expression =
  CastExpression
    <$> (Token.cast *> Token.leftParenthesis *> expression)
    <*> (Token.as *> Token.identifier)

newtype ColumnAlias
  = ColumnAlias Text

columnAlias :: Parser r ColumnAlias
columnAlias =
  ColumnAlias <$> undefined

-- | https://sqlite.org/syntax/column-constraint.html
data ColumnConstraint = ColumnConstraint
  { name :: Maybe Text,
    constraint :: ColumnConstraintType
  }

makeColumnConstraint :: Parser r ColumnConstraintType -> Parser r ColumnConstraint
makeColumnConstraint columnConstraintType =
  ColumnConstraint
    <$> optional (Token.constraint *> Token.identifier)
    <*> columnConstraintType

data ColumnConstraintType
  = ColumnConstraintType'Check Expression
  | ColumnConstraintType'Collate Text
  | ColumnConstraintType'Default Default
  | ColumnConstraintType'ForeignKey ForeignKeyClause
  | ColumnConstraintType'Generated Expression (Maybe GeneratedType)
  | ColumnConstraintType'NotNull (Maybe OnConflictClause)
  | ColumnConstraintType'PrimaryKey (Maybe Ordering) (Maybe OnConflictClause) Bool
  | ColumnConstraintType'Unique (Maybe OnConflictClause)

makeColumnConstraintType :: Parser r Expression -> Parser r ColumnConstraintType
makeColumnConstraintType expression =
  choice
    [ ColumnConstraintType'Check
        <$> (Token.check *> parens expression),
      ColumnConstraintType'Collate
        <$> (Token.collate *> Token.identifier),
      ColumnConstraintType'Default
        <$> choice
          [ Default'Expression <$> parens expression,
            Default'LiteralValue <$> literalValue,
            Default'SignedNumber <$> signedNumber
          ],
      ColumnConstraintType'ForeignKey
        <$> foreignKeyClause,
      ColumnConstraintType'Generated
        <$> (optional (Token.generated *> Token.always) *> Token.as *> parens expression)
        <*> optional generatedType,
      ColumnConstraintType'NotNull
        <$> (Token.not *> Token.null *> optional onConflictClause),
      ColumnConstraintType'PrimaryKey
        <$> (Token.primary *> Token.key *> optional orderingParser)
        <*> optional onConflictClause
        <*> perhaps Token.autoincrement,
      ColumnConstraintType'Unique
        <$> (Token.unique *> optional onConflictClause)
    ]

-- | https://sqlite.org/syntax/column-def.html
data ColumnDefinition = ColumnDefinition
  { name :: Text,
    type_ :: Maybe Text,
    constraints :: [ColumnConstraint]
  }

makeColumnDefinition :: Parser r ColumnConstraint -> Parser r ColumnDefinition
makeColumnDefinition columnConstraint =
  ColumnDefinition
    <$> Token.identifier
    <*> optional Token.identifier
    <*> many columnConstraint

-- https://sqlite.org/syntax/commit-stmt.html
commitStatement :: Parser r (Maybe Token)
commitStatement =
  choice [Token.commit, Token.end] *> optional Token.transaction

commonTableExpression :: Parser r CommonTableExpression
commonTableExpression = undefined

compoundOperator :: Parser r CompoundOperator
compoundOperator =
  choice
    [ CompoundOperator'Except <$ Token.except,
      CompoundOperator'Intersect <$ Token.intersect,
      CompoundOperator'Union <$ Token.union,
      CompoundOperator'UnionAll <$ (Token.union *> Token.all)
    ]

-- | https://sqlite.org/syntax/create-index-stmt.html
data CreateIndexStatement = CreateIndexStatement
  { unique :: Bool,
    ifNotExists :: Bool,
    name :: SchemaQualified Text,
    table :: Text,
    columns :: NonEmpty IndexedColumn,
    where_ :: Maybe Expression
  }

makeCreateIndexStatement :: Parser r Expression -> Parser r IndexedColumn -> Parser r CreateIndexStatement
makeCreateIndexStatement expression indexedColumn =
  CreateIndexStatement
    <$> (Token.create *> perhaps Token.unique)
    <*> (Token.index *> perhaps (Token.if_ *> Token.not *> Token.exists))
    <*> schemaQualified Token.identifier
    <*> (Token.on *> Token.identifier)
    <*> parens (commaSep1 indexedColumn)
    <*> optional (Token.where_ *> expression)

-- | https://sqlite.org/syntax/create-table-stmt.html
data CreateTableStatement = CreateTableStatement
  { temporary :: Bool,
    ifNotExists :: Bool,
    name :: SchemaQualified Text,
    definition :: Either SelectStatement TableDefinition
  }

makeCreateTableStatement ::
  Parser r ColumnDefinition ->
  Parser r Expression ->
  Parser r IndexedColumn ->
  Parser r SelectStatement ->
  Parser r CreateTableStatement
makeCreateTableStatement columnDefinition expression indexedColumn selectStatement =
  CreateTableStatement
    <$> (Token.create *> perhaps (choice [Token.temp, Token.temporary]))
    <*> (Token.table *> perhaps (Token.if_ *> Token.not *> Token.exists))
    <*> schemaQualified Token.identifier
    <*> choice
      [ Left <$> (Token.as *> selectStatement),
        Right <$> tableDefinition
      ]
  where
    tableDefinition =
      makeTableDefinition columnDefinition expression indexedColumn

data Default
  = Default'Expression Expression
  | Default'LiteralValue LiteralValue
  | Default'SignedNumber SignedNumber

runG :: (forall r. Earley.Grammar r (Parser r a)) -> [Token] -> [a]
runG parser tokens =
  fst (Earley.fullParses (Earley.parser parser) tokens)

makeExpression :: Parser r SelectStatement -> Earley.Grammar r (Parser r Expression)
makeExpression selectStatement = mdo
  filterClause <- Earley.rule (makeFilterClause expression)
  functionArguments <- Earley.rule (makeFunctionArguments expression)

  expression <-
    Earley.rule do
      choice
        [ Expression'Not <$> expression,
          expression0
        ]

  expression0 <- rule oink0 expression1
  expression1 <- rule oink1 expression2
  expression2 <- rule (oink2 expression) expression3
  expression3 <- rule oink3 expression4
  expression4 <- rule oink4 expression5
  expression5 <- rule oink5 expression6
  expression6 <- rule oink6 expression7
  expression7 <- rule oink7 expression8
  expression8 <- rule oink8 expression9
  expression9 <- rule oink9 expression10

  expression10 <-
    Earley.rule do
      choice
        [ Expression'AggregateFunctionCall
            <$> Token.identifier
            <*> parens (optional functionArguments)
            <*> optional filterClause,
          Expression'BindParameter <$> bindParameter,
          Expression'Case
            <$> (Token.case_ *> optional expression)
            <*> some ((,) <$> (Token.when *> expression) <*> (Token.then_ *> expression))
            <*> (optional (Token.else_ *> expression) <* Token.end),
          Expression'Cast <$> makeCastExpression expression,
          Expression'Column <$> schemaQualified (tableQualified Token.identifier),
          Expression'Exists <$> (Token.exists *> parens selectStatement),
          Expression'FunctionCall <$> Token.identifier <*> parens (optional functionArguments),
          Expression'LiteralValue <$> literalValue,
          Expression'RaiseFunction <$> raiseFunction,
          Expression'RowValue <$> makeRowValue expression,
          Expression'Subquery <$> parens selectStatement,
          Expression'WindowFunctionCall
            <$> Token.identifier
            <*> parens (optional functionArguments)
            <*> optional filterClause
            <*> overClause,
          parens expression
        ]

  pure expression
  where
    rule oink next = mdo
      e <- Earley.rule (choice (oink e next))
      pure e

    oink0 e1 e2 =
      [ Expression'Or <$> e1 <*> (Token.or *> e2),
        e2
      ]

    oink1 e1 e2 =
      [ Expression'And <$> e1 <*> (Token.and *> e2),
        e2
      ]

    oink2 e0 e1 e2 =
      [ (\x0 not_ x1 x2 -> (if not_ then Expression'NotBetween else Expression'Between) x0 x1 x2)
          <$> e1
          <*> perhaps Token.not
          <*> (Token.between *> e0)
          <*> (Token.and *> e2),
        Expression'Equals <$> e1 <*> (Token.equalsSign *> e2),
        Expression'Equals <$> e1 <*> (Token.equalsSignEqualsSign *> e2),
        (\x0 not_ x1 x2 -> (if not_ then Expression'NotGlob else Expression'Glob) x0 x1 x2)
          <$> e1
          <*> perhaps Token.not
          <*> (Token.glob *> e2)
          <*> optional (Token.escape *> e2),
        (\x0 not_ x1 -> (if not_ then Expression'Not else id) (Expression'InFunction (InFunctionExpression x0 x1)))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> functionCallParser (commaSep0 e0)),
        (\x0 not_ x1 -> (if not_ then Expression'Not else id) (Expression'InSubquery (InSubqueryExpression x0 x1)))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> parens selectStatement),
        (\x0 not_ x1 -> (if not_ then Expression'Not else id) (Expression'InTable (InTableExpression x0 x1)))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> schemaQualified Token.identifier),
        (\x0 not_ x1 -> (if not_ then Expression'Not else id) (Expression'InValues (InValuesExpression x0 x1)))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> parens (commaSep0 e0)),
        (\x0 not_ x1 -> (if not_ then Expression'IsNot else Expression'Is) x0 x1)
          <$> e1
          <*> (Token.is *> perhaps Token.not)
          <*> e2,
        (\x0 not_ x1 x2 -> (if not_ then Expression'NotLike else Expression'Like) x0 x1 x2)
          <$> e1
          <*> perhaps Token.not
          <*> (Token.like *> e2)
          <*> optional (Token.escape *> e2),
        (\x0 not_ x1 x2 -> (if not_ then Expression'NotMatch else Expression'Match) x0 x1 x2)
          <$> e1
          <*> perhaps Token.not
          <*> (Token.match *> e2)
          <*> optional (Token.escape *> e2),
        Expression'NotEquals <$> e1 <*> (Token.exclamationMarkEqualsSign *> e2),
        Expression'NotEquals <$> e1 <*> (Token.lessThanSignGreaterThanSign *> e2),
        (\x0 not_ x1 x2 -> (if not_ then Expression'NotRegexp else Expression'Regexp) x0 x1 x2)
          <$> e1
          <*> perhaps Token.not
          <*> (Token.regexp *> e2)
          <*> optional (Token.escape *> e2),
        (\x0 -> Expression'Is x0 Expression'Null) <$> (e1 <* Token.isnull),
        (\x0 -> Expression'IsNot x0 Expression'Null) <$> (e1 <* Token.notnull),
        (\x0 -> Expression'IsNot x0 Expression'Null) <$> (e1 <* (Token.not *> Token.null)),
        e2
      ]

    oink3 e1 e2 =
      [ Expression'GreaterThan <$> e1 <*> (Token.greaterThanSign *> e2),
        Expression'GreaterThanOrEquals <$> e1 <*> (Token.greaterThanSignEqualsSign *> e2),
        Expression'LessThan <$> e1 <*> (Token.lessThanSign *> e2),
        Expression'LessThanOrEquals <$> e1 <*> (Token.lessThanSignEqualsSign *> e2),
        e2
      ]

    oink4 e1 e2 =
      [ Expression'BitwiseAnd <$> e1 <*> (Token.ampersand *> e2),
        Expression'BitwiseOr <$> e1 <*> (Token.verticalLine *> e2),
        Expression'ShiftLeft <$> e1 <*> (Token.lessThanSignLessThanSign *> e2),
        Expression'ShiftRight <$> e1 <*> (Token.greaterThanSignGreaterThanSign *> e2),
        e2
      ]

    oink5 e1 e2 =
      [ Expression'Minus <$> e1 <*> (Token.hyphenMinus *> e2),
        Expression'Plus <$> e1 <*> (Token.plusSign *> e2),
        e2
      ]

    oink6 e1 e2 =
      [ Expression'Divide <$> e1 <*> (Token.solidus *> e2),
        Expression'Modulo <$> e1 <*> (Token.percentSign *> e2),
        Expression'Multiply <$> e1 <*> (Token.asterisk *> e2),
        e2
      ]

    oink7 e1 e2 =
      [ Expression'Concatenate <$> e1 <*> (Token.verticalLineVerticalLine *> e2),
        e2
      ]

    oink8 e1 e2 =
      [ Expression'BitwiseNegate <$> (Token.tilde *> e1),
        Expression'Negate <$> (Token.hyphenMinus *> e1),
        Token.plusSign *> e1,
        e2
      ]

    oink9 e1 e2 =
      [ Expression'Collate <$> (CollateExpression <$> e1 <*> (Token.collate *> Token.identifier)),
        e2
      ]

makeFilterClause :: Parser r Expression -> Parser r FilterClause
makeFilterClause expression =
  FilterClause
    <$> (Token.filter *> parens (Token.where_ *> expression))

-- | https://sqlite.org/syntax/foreign-key-clause.html
data ForeignKeyClause

foreignKeyClause :: Parser r ForeignKeyClause
foreignKeyClause = undefined

fromClause :: Parser r FromClause
fromClause = undefined

makeFunctionArguments :: Parser r Expression -> Parser r FunctionArguments
makeFunctionArguments expression =
  choice
    [ FunctionArguments'Arguments <$> perhaps Token.distinct <*> commaSep1 expression,
      FunctionArguments'Wildcard <$ Token.asterisk
    ]

functionCallParser :: Parser r (f Expression) -> Parser r (FunctionCall f)
functionCallParser arguments =
  FunctionCall
    <$> schemaQualified Token.identifier
    <*> parens arguments

data FunctionName

functionName :: Parser r FunctionName
functionName = undefined

data GeneratedType
  = GeneratedType'Stored
  | GeneratedType'Virtual

generatedType :: Parser r GeneratedType
generatedType =
  choice
    [ GeneratedType'Stored <$ Token.stored,
      GeneratedType'Virtual <$ Token.virtual
    ]

makeGroupByClause :: Parser r Expression -> Parser r GroupByClause
makeGroupByClause expression =
  GroupByClause
    <$> (Token.group *> Token.by *> commaSep1 expression)
    <*> optional (Token.having *> expression)

newtype IndexOrTableName
  = IndexOrTableName Text

indexOrTableName :: Parser r IndexOrTableName
indexOrTableName =
  IndexOrTableName <$> Token.identifier

-- | https://sqlite.org/syntax/indexed-column.html
data IndexedColumn = IndexedColumn
  { column :: Either Text Expression,
    collation :: Maybe Text,
    ordering :: Maybe Ordering
  }

makeIndexedColumn :: Parser r Expression -> Parser r IndexedColumn
makeIndexedColumn expression =
  IndexedColumn
    <$> choice
      [ Left <$> Token.identifier,
        Right <$> expression
      ]
    <*> optional (Token.collate *> Token.identifier)
    <*> optional orderingParser

makeLimitClause :: Parser r Expression -> Parser r LimitClause
makeLimitClause expression =
  munge
    <$> (Token.limit *> expression)
    <*> optional
      ( choice
          [ Left <$> (Token.offset *> expression),
            Right <$> (Token.comma *> expression)
          ]
      )
  where
    munge :: Expression -> Maybe (Either Expression Expression) -> LimitClause
    munge e1 = \case
      Nothing -> LimitClause e1 Nothing -- LIMIT x
      Just (Left e2) -> LimitClause e1 (Just e2) -- LIMIT x OFFSET y
      Just (Right e2) -> LimitClause e2 (Just e1) -- LIMIT y, x

literalValue :: Parser r LiteralValue
literalValue =
  choice
    [ LiteralValue'Blob <$> Token.blob,
      LiteralValue'CurrentDate <$ Token.currentDate,
      LiteralValue'CurrentTime <$ Token.currentTime,
      LiteralValue'CurrentTimestamp <$ Token.currentTimestamp,
      LiteralValue'False <$ Token.false,
      LiteralValue'Null <$ Token.null,
      LiteralValue'Number <$> Token.number,
      LiteralValue'String <$> Token.string,
      LiteralValue'True <$ Token.true
    ]

-- | https://sqlite.org/syntax/conflict-clause.html
data OnConflictClause
  = OnConflictClause'Abort
  | OnConflictClause'Fail
  | OnConflictClause'Ignore
  | OnConflictClause'Replace
  | OnConflictClause'Rollback

onConflictClause :: Parser r OnConflictClause
onConflictClause =
  Token.on
    *> Token.conflict
    *> choice
      [ OnConflictClause'Abort <$ Token.abort,
        OnConflictClause'Fail <$ Token.fail,
        OnConflictClause'Ignore <$ Token.ignore,
        OnConflictClause'Replace <$ Token.replace,
        OnConflictClause'Rollback <$ Token.rollback
      ]

data Ordering
  = Ordering'Asc
  | Ordering'Desc

orderingParser :: Parser r Ordering
orderingParser =
  choice
    [ Ordering'Asc <$ Token.asc,
      Ordering'Desc <$ Token.desc
    ]

overClause :: Parser r OverClause
overClause =
  Token.over
    *> choice
      [ OverClause'WindowDefinition <$> windowDefinition,
        OverClause'WindowName <$> Token.identifier
      ]

-- | https://sqlite.org/syntax/qualified-table-name.html
data QualifiedTableName

raiseFunction :: Parser r RaiseFunction
raiseFunction =
  Token.raise *> parens (choice xs)
  where
    xs =
      [ RaiseFunction'Abort <$> errorMessage Token.abort,
        RaiseFunction'Fail <$> errorMessage Token.fail,
        RaiseFunction'Ignore <$ Token.ignore,
        RaiseFunction'Rollback <$> errorMessage Token.rollback
      ]
    errorMessage p =
      p *> Token.comma *> Token.string

-- | https://sqlite.org/syntax/returning-clause.html
newtype ReturningClause
  = ReturningClause (NonEmpty ReturningClauseItem)

makeReturningClause :: Parser r ReturningClauseItem -> Parser r ReturningClause
makeReturningClause = undefined

data ReturningClauseItem
  = ReturningClauseItem'All
  | ReturningClauseItem'Expression Expression (Maybe ColumnAlias)

makeReturningClauseItem :: Parser r Expression -> Parser r ReturningClauseItem
makeReturningClauseItem = undefined

-- | https://sqlite.org/syntax/rollback-stmt.html
newtype RollbackStatement
  = RollbackStatement (Maybe SavepointName)

rollbackStatement :: Parser r RollbackStatement
rollbackStatement =
  RollbackStatement
    <$> ( Token.rollback
            *> optional Token.transaction
            *> optional (Token.to *> optional Token.savepoint *> savepointName)
        )

makeRowValue :: Parser r Expression -> Parser r RowValue
makeRowValue expression =
  RowValue
    <$> (Token.leftParenthesis *> expression)
    <*> (Token.comma *> expression)
    <*> many (Token.comma *> expression)

newtype SavepointName
  = SavepointName Text

savepointName :: Parser r SavepointName
savepointName = undefined

schemaQualified :: Parser r a -> Parser r (SchemaQualified a)
schemaQualified p =
  SchemaQualified
    <$> (optional Token.identifier <* Token.fullStop)
    <*> p

makeSelect :: Parser r Expression -> Parser r Select
makeSelect expression =
  Select
    <$> ( Token.select
            *> choice
              [ True <$ Token.distinct,
                False <$ Token.all,
                pure False
              ]
        )
    <*> commaSep1 (schemaQualified (tableQualified Token.identifier))
    <*> optional fromClause
    <*> optional (Token.where_ *> expression)
    <*> optional groupByClause
    <*> optional windowClause
  where
    groupByClause =
      makeGroupByClause expression

makeSelectCore :: Parser r Expression -> Parser r SelectCore
makeSelectCore expression =
  choice
    [ SelectCore'Select <$> select,
      SelectCore'Values <$> (Token.values *> commaSep1 (parens (commaSep1 expression)))
    ]
  where
    select =
      makeSelect expression

makeSelectStatement :: Parser r Expression -> Parser r SelectStatement
makeSelectStatement expression =
  SelectStatement
    <$> withClause
    <*> ((,) <$> selectCore <*> many ((,) <$> compoundOperator <*> selectCore))
    <*> optional undefined
    <*> optional limitClause
  where
    limitClause =
      makeLimitClause expression

    selectCore =
      makeSelectCore expression

data Sign
  = Sign'HyphenMinus
  | Sign'PlusSign

sign :: Parser r Sign
sign =
  choice
    [ Sign'HyphenMinus <$ Token.hyphenMinus,
      Sign'PlusSign <$ Token.plusSign
    ]

-- | https://sqlite.org/syntax/signed-number.html
data SignedNumber
  = SignedNumber (Maybe Sign) Text

signedNumber :: Parser r SignedNumber
signedNumber =
  SignedNumber
    <$> optional sign
    <*> Token.number

data TableAlteration
  = TableAlteration'AddColumn ColumnDefinition
  | TableAlteration'DropColumn Text
  | TableAlteration'Rename Text
  | TableAlteration'RenameColumn Text Text

-- | https://sqlite.org/syntax/table-constraint.html
data TableConstraint = TableConstraint
  { name :: Maybe Text,
    constraint :: TableConstraintType
  }

makeTableConstraint :: Parser r Expression -> Parser r IndexedColumn -> Parser r TableConstraint
makeTableConstraint expression indexedColumn =
  TableConstraint
    <$> optional (Token.constraint *> Token.identifier)
    <*> makeTableConstraintType expression indexedColumn

data TableConstraintType
  = TableConstraintType'Check Expression
  | TableConstraintType'ForeignKey (NonEmpty Text) ForeignKeyClause
  | TableConstraintType'PrimaryKey (NonEmpty IndexedColumn) (Maybe OnConflictClause)
  | TableConstraintType'Unique (NonEmpty IndexedColumn) (Maybe OnConflictClause)

makeTableConstraintType :: Parser r Expression -> Parser r IndexedColumn -> Parser r TableConstraintType
makeTableConstraintType expression indexedColumn =
  choice
    [ TableConstraintType'Check
        <$> (Token.check *> parens expression),
      TableConstraintType'ForeignKey
        <$> (Token.foreign_ *> Token.key *> parens (commaSep1 Token.identifier))
        <*> foreignKeyClause,
      TableConstraintType'PrimaryKey
        <$> (Token.primary *> Token.key *> parens (commaSep1 indexedColumn))
        <*> optional onConflictClause,
      TableConstraintType'Unique
        <$> (Token.unique *> parens (commaSep1 indexedColumn))
        <*> optional onConflictClause
    ]

data TableDefinition
  = TableDefinition (NonEmpty ColumnDefinition) [TableConstraint] Bool

makeTableDefinition ::
  Parser r ColumnDefinition ->
  Parser r Expression ->
  Parser r IndexedColumn ->
  Parser r TableDefinition
makeTableDefinition columnDefinition expression indexedColumn =
  TableDefinition
    <$> (Token.leftParenthesis *> commaSep1 columnDefinition)
    <*> (many (Token.comma *> tableConstraint) <* Token.rightParenthesis)
    <*> perhaps (Token.without *> Token.rowid)
  where
    tableConstraint =
      makeTableConstraint expression indexedColumn

tableQualified :: Parser r a -> Parser r (TableQualified a)
tableQualified p =
  TableQualified
    <$> (optional Token.identifier <* Token.fullStop)
    <*> p

data TransactionType
  = TransactionType'Deferred
  | TransactionType'Exclusive
  | TransactionType'Immediate

transactionType :: Parser r TransactionType
transactionType =
  choice
    [ TransactionType'Deferred <$ Token.deferred,
      TransactionType'Exclusive <$ Token.exclusive,
      TransactionType'Immediate <$ Token.immediate
    ]

windowClause :: Parser r WindowClause
windowClause = undefined

windowDefinition :: Parser r WindowDefinition
windowDefinition = undefined

withClause :: Parser r WithClause
withClause =
  WithClause
    <$> (Token.with *> perhaps Token.recursive)
    <*> commaSep1 commonTableExpression
