module Sqlite.Syntax.Parser where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Control.Applicative.Combinators.NonEmpty (some)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Sqlite.Syntax.Parser.Token as Token
import Sqlite.Syntax.Token (Token)
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, not, null)

-- TODO simplify some things with defaults (e.g. missing distinct/all == all)
-- TODO derive show/eq/generic
-- TODO flatten some things? e.g. UnaryOperator -> flatten into multiple Expression
-- TODO move signed-number to lexer probably
-- TODO FunctionName, etc. -> Identifier

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

data Syntax
  = Syntax'AlterTableStatement AlterTableStatement
  | Syntax'AnalyzeStatement AnalyzeStatement
  | Syntax'AttachStatement AttachStatement
  | Syntax'BeginStatement BeginStatement
  | Syntax'CommitStatement
  | Syntax'CreateIndexStatement CreateIndexStatement
  | Syntax'CreateTableStatement CreateTableStatement
  | Syntax'CreateTriggerStatement TODO
  | Syntax'CreateViewStatement TODO
  | Syntax'CreateVirtualTableStatement TODO
  | Syntax'DeleteStatement TODO
  | Syntax'DetachStatement TODO
  | Syntax'DropIndexStatement TODO
  | Syntax'DropTableStatement TODO
  | Syntax'DropTriggerStatement TODO
  | Syntax'DropViewStatement TODO
  | Syntax'InsertStatement TODO
  | Syntax'PragmaStatement TODO
  | Syntax'ReindexStatement TODO
  | Syntax'ReleaseStatement TODO
  | Syntax'RollbackStatement RollbackStatement
  | Syntax'SavepointStatement TODO
  | Syntax'SelectStatement TODO
  | Syntax'UpdateStatement TODO
  | Syntax'VacuumStatement TODO

data TODO

syntax :: Earley.Grammar r (Parser r Syntax)
syntax = mdo
  columnDefinition <- Earley.rule (makeColumnDefinition columnConstraint)
  expression <- makeExpression selectStatement
  indexedColumn <- Earley.rule (makeIndexedColumn expression)
  selectStatement <- undefined

  let alterTableStatement = makeAlterTableStatement columnDefinition
      attachStatement = makeAttachStatement expression
      columnConstraint = makeColumnConstraint columnConstraintType
      columnConstraintType = makeColumnConstraintType expression
      createIndexStatement = makeCreateIndexStatement expression indexedColumn
      createTableStatement = makeCreateTableStatement columnDefinition expression indexedColumn selectStatement

  pure do
    choice
      [ Syntax'AlterTableStatement <$> alterTableStatement,
        Syntax'AnalyzeStatement <$> analyzeStatement,
        Syntax'AttachStatement <$> attachStatement,
        Syntax'BeginStatement <$> beginStatement,
        -- https://sqlite.org/syntax/commit-stmt.html
        Syntax'CommitStatement <$ (choice [Token.commit, Token.end] *> optional Token.transaction),
        Syntax'CreateIndexStatement <$> createIndexStatement,
        Syntax'CreateTableStatement <$> createTableStatement,
        Syntax'RollbackStatement <$> rollbackStatement
      ]

--

-- | https://sqlite.org/lang_altertable.html
data AlterTableStatement
  = AlterTableStatement (SchemaQualified TableName) TableAlteration

makeAlterTableStatement :: Parser r ColumnDefinition -> Parser r AlterTableStatement
makeAlterTableStatement columnDefinition =
  AlterTableStatement
    <$> (Token.alter *> Token.table *> schemaQualified tableName)
    <*> choice
      [ TableAlteration'AddColumn
          <$> columnDefinition,
        TableAlteration'DropColumn
          <$> columnName,
        TableAlteration'Rename
          <$> (Token.rename *> Token.to *> tableName),
        TableAlteration'RenameColumn
          <$> (Token.rename *> optional Token.column *> columnName)
          <*> (Token.to *> columnName)
      ]

-- | https://sqlite.org/lang_analyze.html
data AnalyzeStatement
  = AnalyzeStatement (Maybe (SchemaQualified IndexOrTableName))

analyzeStatement :: Parser r AnalyzeStatement
analyzeStatement =
  AnalyzeStatement
    <$> (Token.analyze *> optional (schemaQualified indexOrTableName))

-- | https://sqlite.org/syntax/attach-stmt.html
data AttachStatement
  = AttachStatement Expression SchemaName

makeAttachStatement :: Parser r Expression -> Parser r AttachStatement
makeAttachStatement expression =
  AttachStatement
    <$> (Token.attach *> optional Token.database *> expression)
    <*> (Token.as *> schemaName)

-- | https://sqlite.org/syntax/begin-stmt.html
newtype BeginStatement
  = BeginStatement (Maybe TransactionType)

beginStatement :: Parser r BeginStatement
beginStatement =
  BeginStatement
    <$> (Token.begin *> optional transactionType <* optional Token.transaction)

data BindParameter

bindParameter :: Parser r BindParameter
bindParameter = undefined

newtype ColumnAlias
  = ColumnAlias Text

columnAlias :: Parser r ColumnAlias
columnAlias =
  ColumnAlias <$> undefined

-- | https://sqlite.org/syntax/column-constraint.html
data ColumnConstraint
  = ColumnConstraint (Maybe ConstraintName) ColumnConstraintType

makeColumnConstraint :: Parser r ColumnConstraintType -> Parser r ColumnConstraint
makeColumnConstraint columnConstraintType =
  ColumnConstraint
    <$> optional (Token.constraint *> constraintName)
    <*> columnConstraintType

data ColumnConstraintType
  = ColumnConstraintType'Check Expression
  | ColumnConstraintType'Collate CollationName
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
        <$> (Token.collate *> collationName),
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
        <$> (Token.primary *> Token.key *> optional ordering)
        <*> optional onConflictClause
        <*> perhaps Token.autoincrement,
      ColumnConstraintType'Unique
        <$> (Token.unique *> optional onConflictClause)
    ]

-- | https://sqlite.org/syntax/column-def.html
data ColumnDefinition
  = ColumnDefinition ColumnName (Maybe TypeName) [ColumnConstraint]

makeColumnDefinition :: Parser r ColumnConstraint -> Parser r ColumnDefinition
makeColumnDefinition columnConstraint =
  ColumnDefinition
    <$> columnName
    <*> optional typeName
    <*> many columnConstraint

newtype CollationName
  = CollationName Text

collationName :: Parser r CollationName
collationName =
  CollationName <$> undefined

newtype ColumnName
  = ColumnName Text

columnName :: Parser r ColumnName
columnName =
  ColumnName <$> Token.identifier

data CommonTableExpression

commonTableExpression :: Parser r CommonTableExpression
commonTableExpression = undefined

data CompoundOperator
  = CompoundOperator'Except
  | CompoundOperator'Intersect
  | CompoundOperator'Union
  | CompoundOperator'UnionAll

compoundOperator :: Parser r CompoundOperator
compoundOperator =
  choice
    [ CompoundOperator'Except <$ Token.except,
      CompoundOperator'Intersect <$ Token.intersect,
      CompoundOperator'Union <$ Token.union,
      CompoundOperator'UnionAll <$ (Token.union *> Token.all)
    ]

newtype ConstraintName
  = ConstraintName Text

constraintName :: Parser r ConstraintName
constraintName =
  ConstraintName <$> Token.identifier

-- | https://sqlite.org/syntax/create-index-stmt.html
data CreateIndexStatement
  = CreateIndexStatement Bool Bool (SchemaQualified IndexName) TableName (NonEmpty IndexedColumn) (Maybe Expression)

makeCreateIndexStatement :: Parser r Expression -> Parser r IndexedColumn -> Parser r CreateIndexStatement
makeCreateIndexStatement expression indexedColumn =
  CreateIndexStatement
    <$> (Token.create *> perhaps Token.unique)
    <*> (Token.index *> perhaps (Token.if_ *> Token.not *> Token.exists))
    <*> schemaQualified indexName
    <*> (Token.on *> tableName)
    <*> parens (commaSep1 indexedColumn)
    <*> optional (Token.where_ *> expression)

-- | https://sqlite.org/syntax/create-table-stmt.html
data CreateTableStatement
  = CreateTableStatement Bool Bool (SchemaQualified TableName) (Either SelectStatement TableDefinition)

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
    <*> schemaQualified tableName
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

data Expression
  = Expression'AggregateFunctionCall Text (Maybe FunctionArguments) (Maybe FilterClause)
  | Expression'And Expression Expression
  | Expression'Between Expression Expression Expression
  | Expression'BindParameter BindParameter
  | Expression'BitwiseAnd Expression Expression
  | Expression'BitwiseNegate Expression
  | Expression'BitwiseOr Expression Expression
  | Expression'Case (Maybe Expression) (NonEmpty (Expression, Expression)) (Maybe Expression)
  | Expression'Cast Expression TypeName
  | Expression'Collate Expression CollationName
  | Expression'Column (SchemaQualified (TableQualified ColumnName))
  | Expression'Concatenate Expression Expression
  | Expression'Divide Expression Expression
  | Expression'Equals Expression Expression
  | Expression'Exists SelectStatement
  | Expression'FunctionCall Text (Maybe FunctionArguments)
  | Expression'Glob Expression Expression (Maybe Expression)
  | Expression'GreaterThan Expression Expression
  | Expression'GreaterThanOrEquals Expression Expression
  | Expression'InFunction Expression (SchemaQualified FunctionName) [Expression]
  | Expression'InSubquery Expression SelectStatement
  | Expression'InTable Expression (SchemaQualified TableName)
  | Expression'InValues Expression [Expression]
  | Expression'Is Expression Expression
  | Expression'IsNot Expression Expression
  | Expression'LessThan Expression Expression
  | Expression'LessThanOrEquals Expression Expression
  | Expression'Like Expression Expression (Maybe Expression)
  | Expression'LiteralValue LiteralValue
  | Expression'Match Expression Expression (Maybe Expression)
  | Expression'Minus Expression Expression
  | Expression'Modulo Expression Expression
  | Expression'Multiply Expression Expression
  | Expression'Negate Expression
  | Expression'Not Expression
  | Expression'NotBetween Expression Expression Expression
  | Expression'NotEquals Expression Expression
  | Expression'NotGlob Expression Expression (Maybe Expression)
  | Expression'NotInFunction Expression (SchemaQualified FunctionName) [Expression]
  | Expression'NotInSubquery Expression SelectStatement
  | Expression'NotInTable Expression (SchemaQualified TableName)
  | Expression'NotInValues Expression [Expression]
  | Expression'NotLike Expression Expression (Maybe Expression)
  | Expression'NotMatch Expression Expression (Maybe Expression)
  | Expression'NotRegexp Expression Expression (Maybe Expression)
  | Expression'Null
  | Expression'Or Expression Expression
  | Expression'Plus Expression Expression
  | Expression'RaiseFunction RaiseFunction
  | Expression'Regexp Expression Expression (Maybe Expression)
  | Expression'RowValue RowValue
  | Expression'ShiftLeft Expression Expression
  | Expression'ShiftRight Expression Expression
  | Expression'Subquery SelectStatement
  | Expression'WindowFunctionCall Text (Maybe FunctionArguments) (Maybe FilterClause) OverClause

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
          Expression'Cast <$> (Token.cast *> Token.leftParenthesis *> expression) <*> (Token.as *> typeName),
          Expression'Column <$> schemaQualified (tableQualified columnName),
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
        (\x0 not_ x1 x2 -> (if not_ then Expression'NotInFunction else Expression'InFunction) x0 x1 x2)
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> schemaQualified functionName)
          <*> parens (commaSep0 e0),
        (\x0 not_ x1 -> (if not_ then Expression'NotInSubquery else Expression'InSubquery) x0 x1)
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> parens selectStatement),
        (\x0 not_ x1 -> (if not_ then Expression'NotInTable else Expression'InTable) x0 x1)
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> schemaQualified tableName),
        (\x0 not_ x1 -> (if not_ then Expression'NotInValues else Expression'InValues) x0 x1)
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
      [ Expression'Collate <$> e1 <*> (Token.collate *> collationName),
        e2
      ]

newtype FilterClause
  = FilterClause Expression

makeFilterClause :: Parser r Expression -> Parser r FilterClause
makeFilterClause expression =
  FilterClause
    <$> (Token.filter *> parens (Token.where_ *> expression))

-- | https://sqlite.org/syntax/foreign-key-clause.html
data ForeignKeyClause

foreignKeyClause :: Parser r ForeignKeyClause
foreignKeyClause = undefined

newtype FromClause
  = FromClause (Either (NonEmpty TableOrSubquery) JoinClause)

fromClause :: Parser r FromClause
fromClause = undefined

data FunctionArguments
  = FunctionArguments'Arguments Bool (NonEmpty Expression)
  | FunctionArguments'Wildcard

makeFunctionArguments :: Parser r Expression -> Parser r FunctionArguments
makeFunctionArguments expression =
  choice
    [ FunctionArguments'Arguments <$> perhaps Token.distinct <*> commaSep1 expression,
      FunctionArguments'Wildcard <$ Token.asterisk
    ]

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

data GroupByClause = GroupByClause
  { groupBy :: NonEmpty Expression,
    having :: Maybe HavingClause
  }

makeGroupByClause :: Parser r Expression -> Parser r GroupByClause
makeGroupByClause expression =
  GroupByClause
    <$> (Token.group *> Token.by *> commaSep1 expression)
    <*> optional havingClause
  where
    havingClause =
      makeHavingClause expression

newtype HavingClause
  = HavingClause Expression

makeHavingClause :: Parser r Expression -> Parser r HavingClause
makeHavingClause expression =
  HavingClause
    <$> (Token.having *> expression)

newtype IndexName
  = IndexName Text

indexName :: Parser r IndexName
indexName = undefined

newtype IndexOrTableName
  = IndexOrTableName Text

indexOrTableName :: Parser r IndexOrTableName
indexOrTableName =
  IndexOrTableName <$> Token.identifier

-- | https://sqlite.org/syntax/indexed-column.html
data IndexedColumn
  = IndexedColumn (Either ColumnName Expression) (Maybe CollationName) (Maybe Ordering)

data JoinClause

makeIndexedColumn :: Parser r Expression -> Parser r IndexedColumn
makeIndexedColumn expression =
  IndexedColumn
    <$> choice
      [ Left <$> columnName,
        Right <$> expression
      ]
    <*> optional (Token.collate *> collationName)
    <*> optional ordering

data LimitClause
  = LimitClause Expression (Maybe OffsetClause)

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
      Just (Left e2) -> LimitClause e1 (Just (OffsetClause e2)) -- LIMIT x OFFSET y
      Just (Right e2) -> LimitClause e2 (Just (OffsetClause e1)) -- LIMIT y, x

data LiteralValue
  = LiteralValue'BlobLiteral Text
  | LiteralValue'CurrentDate
  | LiteralValue'CurrentTime
  | LiteralValue'CurrentTimestamp
  | LiteralValue'False
  | LiteralValue'Null
  | LiteralValue'NumericLiteral Text
  | LiteralValue'StringLiteral Text
  | LiteralValue'True

literalValue :: Parser r LiteralValue
literalValue =
  choice
    [ LiteralValue'BlobLiteral <$> Token.blob,
      LiteralValue'CurrentDate <$ Token.currentDate,
      LiteralValue'CurrentTime <$ Token.currentTime,
      LiteralValue'CurrentTimestamp <$ Token.currentTimestamp,
      LiteralValue'False <$ Token.false,
      LiteralValue'Null <$ Token.null,
      LiteralValue'NumericLiteral <$> Token.number,
      LiteralValue'StringLiteral <$> Token.string,
      LiteralValue'True <$ Token.true
    ]

newtype OffsetClause
  = OffsetClause Expression

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

newtype OrderByClause
  = OrderByClause (NonEmpty OrderingTerm)

orderByClause :: Parser r OrderByClause
orderByClause = undefined

data Ordering
  = Ordering'Asc
  | Ordering'Desc

ordering :: Parser r Ordering
ordering =
  choice
    [ Ordering'Asc <$ Token.asc,
      Ordering'Desc <$ Token.desc
    ]

data OrderingTerm

data OverClause
  = OverClause'WindowDefinition WindowDefinition
  | OverClause'WindowName WindowName

overClause :: Parser r OverClause
overClause =
  Token.over
    *> choice
      [ OverClause'WindowDefinition <$> windowDefinition,
        OverClause'WindowName <$> windowName
      ]

-- | https://sqlite.org/syntax/qualified-table-name.html
data QualifiedTableName

-- | https://sqlite.org/syntax/raise-function.html
data RaiseFunction
  = RaiseFunction'Abort Text
  | RaiseFunction'Fail Text
  | RaiseFunction'Ignore
  | RaiseFunction'Rollback Text

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

data ResultColumn

resultColumn :: Parser r ResultColumn
resultColumn = undefined

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

-- | https://www.sqlite.org/rowvalue.html
data RowValue
  = RowValue Expression Expression [Expression]

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

newtype SchemaName
  = SchemaName Text

schemaName :: Parser r SchemaName
schemaName =
  SchemaName <$> Token.identifier

data SchemaQualified a
  = SchemaQualified (Maybe SchemaName) a

schemaQualified :: Parser r a -> Parser r (SchemaQualified a)
schemaQualified p =
  SchemaQualified
    <$> (optional schemaName <* Token.fullStop)
    <*> p

data Select = Select
  { distinct :: Bool,
    columns :: NonEmpty ResultColumn,
    from :: Maybe FromClause,
    where_ :: Maybe WhereClause,
    groupBy :: Maybe GroupByClause,
    window :: Maybe WindowClause
  }

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
    <*> commaSep1 resultColumn
    <*> optional fromClause
    <*> optional whereClause
    <*> optional groupByClause
    <*> optional windowClause
  where
    groupByClause =
      makeGroupByClause expression

    whereClause =
      makeWhereClause expression

-- | https://sqlite.org/syntax/select-core.html
data SelectCore
  = SelectCore'Select Select
  | SelectCore'Values (NonEmpty (NonEmpty Expression))

makeSelectCore :: Parser r Expression -> Parser r SelectCore
makeSelectCore expression =
  choice
    [ SelectCore'Select <$> select,
      SelectCore'Values <$> (Token.values *> commaSep1 (parens (commaSep1 expression)))
    ]
  where
    select =
      makeSelect expression

-- | https://sqlite.org/syntax/select-stmt.html
data SelectStatement = SelectStatement
  { with :: WithClause,
    select :: (SelectCore, [(CompoundOperator, SelectCore)]),
    orderBy :: Maybe OrderByClause,
    limit :: Maybe LimitClause
  }

makeSelectStatement :: Parser r Expression -> Parser r SelectStatement
makeSelectStatement expression =
  SelectStatement
    <$> withClause
    <*> ((,) <$> selectCore <*> many ((,) <$> compoundOperator <*> selectCore))
    <*> optional orderByClause
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
  | TableAlteration'DropColumn ColumnName
  | TableAlteration'Rename TableName
  | TableAlteration'RenameColumn ColumnName ColumnName

-- | https://sqlite.org/syntax/table-constraint.html
data TableConstraint
  = TableConstraint (Maybe ConstraintName) TableConstraintType

makeTableConstraint :: Parser r Expression -> Parser r IndexedColumn -> Parser r TableConstraint
makeTableConstraint expression indexedColumn =
  TableConstraint
    <$> optional (Token.constraint *> constraintName)
    <*> makeTableConstraintType expression indexedColumn

data TableConstraintType
  = TableConstraintType'Check Expression
  | TableConstraintType'ForeignKey (NonEmpty ColumnName) ForeignKeyClause
  | TableConstraintType'PrimaryKey (NonEmpty IndexedColumn) (Maybe OnConflictClause)
  | TableConstraintType'Unique (NonEmpty IndexedColumn) (Maybe OnConflictClause)

makeTableConstraintType :: Parser r Expression -> Parser r IndexedColumn -> Parser r TableConstraintType
makeTableConstraintType expression indexedColumn =
  choice
    [ TableConstraintType'Check
        <$> (Token.check *> parens expression),
      TableConstraintType'ForeignKey
        <$> (Token.foreign_ *> Token.key *> parens (commaSep1 columnName))
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

newtype TableName
  = TableName Text

tableName :: Parser r TableName
tableName =
  TableName <$> Token.identifier

data TableOrSubquery

data TableQualified a
  = TableQualified (Maybe TableName) a

tableQualified :: Parser r a -> Parser r (TableQualified a)
tableQualified p =
  TableQualified
    <$> (optional tableName <* Token.fullStop)
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

newtype TypeName
  = TypeName Text

typeName :: Parser r TypeName
typeName =
  TypeName <$> Token.identifier

newtype WhereClause
  = WhereClause Expression

makeWhereClause :: Parser r Expression -> Parser r WhereClause
makeWhereClause expression =
  WhereClause
    <$> (Token.where_ *> expression)

newtype WindowClause
  = WindowClause (NonEmpty (WindowName, WindowDefinition))

windowClause :: Parser r WindowClause
windowClause = undefined

data WindowDefinition

windowDefinition :: Parser r WindowDefinition
windowDefinition = undefined

data WindowName

windowName :: Parser r WindowName
windowName = undefined

data WithClause = WithClause
  { recursive :: Bool,
    commonTableExpressions :: NonEmpty CommonTableExpression
  }

withClause :: Parser r WithClause
withClause =
  WithClause
    <$> (Token.with *> perhaps Token.recursive)
    <*> commaSep1 commonTableExpression
