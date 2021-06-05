-- TODO export list
-- TODO move types out
module Sqlite.Syntax.Parser where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Control.Applicative.Combinators.NonEmpty (some)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Type.Aliased
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.ForeignKeyClause
import Sqlite.Syntax.Internal.Type.FunctionCall
import Sqlite.Syntax.Internal.Type.LiteralValue
import Sqlite.Syntax.Internal.Type.Named
import Sqlite.Syntax.Internal.Type.OrderingTerm
import Sqlite.Syntax.Internal.Type.QualifiedTableName
import Sqlite.Syntax.Internal.Type.SchemaQualified
import Sqlite.Syntax.Internal.Type.SelectStatement
import Sqlite.Syntax.Internal.Type.TableQualified
import Sqlite.Syntax.Lexer (lex)
import qualified Sqlite.Syntax.Parser.Token as Token
import Sqlite.Syntax.Token (Token)
import qualified Sqlite.Syntax.Token as Token
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, lex, not, null)

-- TODO simplify some things with defaults (e.g. missing distinct/all == all)
-- TODO move signed-number to lexer probably
-- TODO name things *Parser
-- TODO does 'many' need to be 'rule'd?

--

parseStatement :: Text -> Either Text Statement
parseStatement sql = do
  tokens <- lex sql
  case Earley.fullParses statementParser_ tokens of
    ([], report) -> Left (Text.pack (show report))
    ([s], _) -> Right s
    (ss, _) -> Left (Text.unlines (map (Text.pack . show) ss))

statementParser_ :: Earley.Parser Text [Token] Statement
statementParser_ =
  Earley.parser statementParser

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
  | -- | https://sqlite.org/syntax/begin-stmt.html
    Statement'Begin TransactionType
  | -- | https://sqlite.org/syntax/commit-stmt.html
    Statement'Commit
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
  | -- | https://sqlite.org/syntax/rollback-stmt.html
    Statement'Rollback (Maybe Text)
  | Statement'Savepoint TODO
  | Statement'Select SelectStatement
  | Statement'Update TODO
  | Statement'Vacuum TODO
  deriving stock (Eq, Generic, Show)

data TODO = TODO
  deriving stock (Eq, Generic, Show)

statementParser :: Earley.Grammar r (Parser r Statement)
statementParser = mdo
  columnDefinitionParser <- Earley.rule (makeColumnDefinitionParser expressionParser)
  expressionParser <- makeExpression selectStatementParser
  indexedColumnParser <- Earley.rule (makeIndexedColumn expressionParser)
  qualifiedTableNameParser <- Earley.rule undefined
  selectStatementParser <- makeSelectStatementParser expressionParser qualifiedTableNameParser

  let alterTableStatement = makeAlterTableStatement columnDefinitionParser
      attachStatement = makeAttachStatement expressionParser
      createIndexStatement = makeCreateIndexStatement expressionParser indexedColumnParser
      createTableStatement =
        makeCreateTableStatement
          columnDefinitionParser
          expressionParser
          indexedColumnParser
          selectStatementParser

  pure do
    choice
      [ Statement'AlterTable <$> alterTableStatement,
        Statement'Analyze <$> analyzeStatement,
        Statement'Attach <$> attachStatement,
        Statement'Begin
          <$> ( Token.begin
                  *> choice
                    [ TransactionType'Deferred <$ Token.deferred,
                      TransactionType'Exclusive <$ Token.exclusive,
                      TransactionType'Immediate <$ Token.immediate,
                      pure TransactionType'Deferred
                    ]
                  <* optional Token.transaction
              ),
        Statement'Commit <$ (choice [Token.commit, Token.end] *> optional Token.transaction),
        Statement'CreateIndex <$> createIndexStatement,
        Statement'CreateTable <$> createTableStatement,
        Statement'CreateTrigger <$> pure TODO,
        Statement'CreateView <$> pure TODO,
        Statement'CreateVirtualTable <$> pure TODO,
        Statement'Delete <$> pure TODO,
        Statement'Detach <$> pure TODO,
        Statement'DropIndex <$> pure TODO,
        Statement'DropTable <$> pure TODO,
        Statement'DropTrigger <$> pure TODO,
        Statement'DropView <$> pure TODO,
        Statement'Insert <$> pure TODO,
        Statement'Pragma <$> pure TODO,
        Statement'Reindex <$> pure TODO,
        Statement'Release <$> pure TODO,
        Statement'Rollback
          <$> ( Token.rollback
                  *> optional Token.transaction
                  *> optional (Token.to *> optional Token.savepoint *> Token.identifier)
              ),
        Statement'Savepoint <$> pure TODO,
        Statement'Select <$> selectStatementParser,
        Statement'Update <$> pure TODO,
        Statement'Vacuum <$> pure TODO
      ]

--

-- aliasedParser :: Parser r a -> Parser r (Aliased a)
-- aliasedParser parser =
--   Aliased <$> parser <*> optional (Token.as *> Token.identifier)

-- | https://sqlite.org/lang_altertable.html
data AlterTableStatement = AlterTableStatement
  { table :: SchemaQualified Text,
    alteration :: TableAlteration
  }
  deriving stock (Eq, Generic, Show)

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
  = AnalyzeStatement (Maybe (SchemaQualified Text))
  deriving stock (Eq, Generic, Show)

analyzeStatement :: Parser r AnalyzeStatement
analyzeStatement =
  AnalyzeStatement <$> (Token.analyze *> optional (schemaQualified Token.identifier))

-- | https://sqlite.org/syntax/attach-stmt.html
data AttachStatement = AttachStatement
  { database :: Expression,
    schema :: Text
  }
  deriving stock (Eq, Generic, Show)

makeAttachStatement :: Parser r Expression -> Parser r AttachStatement
makeAttachStatement expression =
  AttachStatement <$> (Token.attach *> optional Token.database *> expression) <*> (Token.as *> Token.identifier)

bindParameter :: Parser r BindParameter
bindParameter = undefined

data ColumnConstraint
  = ColumnConstraint'Check Expression
  | ColumnConstraint'Collate Text
  | ColumnConstraint'Default Default
  | ColumnConstraint'ForeignKey ForeignKeyClause
  | ColumnConstraint'Generated Expression (Maybe GeneratedType)
  | ColumnConstraint'NotNull (Maybe OnConflict)
  | ColumnConstraint'PrimaryKey Ordering (Maybe OnConflict) Bool
  | ColumnConstraint'Unique (Maybe OnConflict)
  deriving stock (Eq, Generic, Show)

-- | https://sqlite.org/syntax/column-def.html
data ColumnDefinition = ColumnDefinition
  { name :: Text,
    type_ :: Maybe Text,
    constraints :: [Named ColumnConstraint]
  }
  deriving stock (Eq, Generic, Show)

makeColumnDefinitionParser :: forall r. Parser r Expression -> Parser r ColumnDefinition
makeColumnDefinitionParser expressionParser =
  ColumnDefinition
    <$> Token.identifier
    <*> optional Token.identifier
    <*> many namedColumnConstraintParser
  where
    namedColumnConstraintParser :: Parser r (Named ColumnConstraint)
    namedColumnConstraintParser =
      Named
        <$> optional (Token.constraint *> Token.identifier)
        <*> columnConstraintParser
      where
        columnConstraintParser :: Parser r ColumnConstraint
        columnConstraintParser =
          choice
            [ ColumnConstraint'Check <$> (Token.check *> parens expressionParser),
              ColumnConstraint'Collate <$> (Token.collate *> Token.identifier),
              ColumnConstraint'Default
                <$> choice
                  [ Default'Expression <$> parens expressionParser,
                    Default'LiteralValue <$> literalValue,
                    Default'SignedNumber <$> signedNumber
                  ],
              ColumnConstraint'ForeignKey <$> foreignKeyClauseParser,
              ColumnConstraint'Generated
                <$> (optional (Token.generated *> Token.always) *> Token.as *> parens expressionParser)
                <*> optional generatedType,
              ColumnConstraint'NotNull <$> (Token.not *> Token.null *> optional onConflictParser),
              ColumnConstraint'PrimaryKey
                <$> (Token.primary *> Token.key *> orderingParser)
                <*> optional onConflictParser
                <*> perhaps Token.autoincrement,
              ColumnConstraint'Unique <$> (Token.unique *> optional onConflictParser)
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
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Eq, Generic, Show)

makeCreateTableStatement ::
  forall r.
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
        Right <$> tableDefinitionParser
      ]
  where
    tableDefinitionParser :: Parser r TableDefinition
    tableDefinitionParser =
      TableDefinition
        <$> (Token.leftParenthesis *> commaSep1 columnDefinition)
        <*> many (Token.comma *> namedTableConstraintParser)
        <* Token.rightParenthesis
        <*> perhaps (Token.without *> Token.rowid)
      where
        namedTableConstraintParser :: Parser r (Named TableConstraint)
        namedTableConstraintParser =
          Named
            <$> optional (Token.constraint *> Token.identifier)
            <*> tableConstraintParser
          where
            tableConstraintParser :: Parser r TableConstraint
            tableConstraintParser =
              choice
                [ TableConstraint'Check <$> (Token.check *> parens expression),
                  TableConstraint'ForeignKey
                    <$> (Token.foreign_ *> Token.key *> parens (commaSep1 Token.identifier))
                    <*> foreignKeyClauseParser,
                  TableConstraint'PrimaryKey
                    <$> (Token.primary *> Token.key *> parens (commaSep1 indexedColumn))
                    <*> optional onConflictParser,
                  TableConstraint'Unique
                    <$> (Token.unique *> parens (commaSep1 indexedColumn))
                    <*> optional onConflictParser
                ]

data Default
  = Default'Expression Expression
  | Default'LiteralValue LiteralValue
  | Default'SignedNumber SignedNumber
  deriving stock (Eq, Generic, Show)

runG :: (forall r. Earley.Grammar r (Parser r a)) -> [Token] -> [a]
runG parser tokens =
  fst (Earley.fullParses (Earley.parser parser) tokens)

makeExpression :: Parser r SelectStatement -> Earley.Grammar r (Parser r Expression)
makeExpression selectStatement = mdo
  filterClauseParser <- Earley.rule (Token.filter *> parens (Token.where_ *> expression))

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
            <$> ( AggregateFunctionCall
                    <$> functionCallParser
                      ( choice
                          [ AggregateFunctionArguments'Arguments <$> perhaps Token.distinct <*> commaSep1 expression,
                            AggregateFunctionArguments'Wildcard <$ Token.asterisk,
                            pure AggregateFunctionArguments'None
                          ]
                      )
                    <*> optional filterClauseParser
                ),
          Expression'BindParameter <$> bindParameter,
          Expression'Case
            <$> ( CaseExpression
                    <$> (Token.case_ *> optional expression)
                    <*> some ((,) <$> (Token.when *> expression) <*> (Token.then_ *> expression))
                    <*> choice
                      [ Token.else_ *> expression,
                        pure (Expression'LiteralValue LiteralValue'Null)
                      ]
                    <* Token.end
                ),
          Expression'Cast
            <$> ( CastExpression
                    <$> (Token.cast *> Token.leftParenthesis *> expression)
                    <*> (Token.as *> Token.identifier)
                ),
          Expression'Column <$> schemaQualified (tableQualified Token.identifier),
          Expression'Exists <$> (Token.exists *> parens selectStatement),
          Expression'FunctionCall <$> simpleFunctionCallParser,
          Expression'LiteralValue <$> literalValue,
          Expression'Raise <$> raiseParser,
          Expression'RowValue
            <$> ( RowValue
                    <$> (Token.leftParenthesis *> expression)
                    <*> (Token.comma *> expression)
                    <*> many (Token.comma *> expression)
                ),
          Expression'Subquery <$> parens selectStatement,
          Expression'WindowFunctionCall
            <$> ( WindowFunctionCall
                    <$> simpleFunctionCallParser
                    <*> optional filterClauseParser
                    <*> overClauseParser
                ),
          parens expression
        ]

  simpleFunctionCallParser <-
    Earley.rule do
      functionCallParser
        ( choice
            [ FunctionArguments'Arguments <$> commaSep0 expression,
              FunctionArguments'Wildcard <$ Token.asterisk
            ]
        )

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
      [ (\x0 not_ x1 x2 -> (if not_ then Expression'Not else id) (Expression'Between x0 x1 x2))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.between *> e0)
          <*> (Token.and *> e2),
        Expression'Equals <$> e1 <*> (choice [Token.equalsSign, Token.equalsSignEqualsSign] *> e2),
        (\x0 not_ x1 x2 -> (if not_ then Expression'Not else id) (Expression'Glob x0 x1 x2))
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
        (\x0 not_ x1 -> (if not_ then Expression'Not else id) (Expression'Is x0 x1))
          <$> e1
          <*> (Token.is *> perhaps Token.not)
          <*> e2,
        (\x0 -> Expression'Is x0 (Expression'LiteralValue LiteralValue'Null)) <$> e1 <* Token.isnull,
        (\x0 -> Expression'Not (Expression'Is x0 (Expression'LiteralValue LiteralValue'Null)))
          <$> e1
          <* choice [Token.not *> Token.null, Token.notnull],
        (\x0 not_ x1 x2 -> (if not_ then Expression'Not else id) (Expression'Like x0 x1 x2))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.like *> e2)
          <*> optional (Token.escape *> e2),
        (\x0 not_ x1 x2 -> (if not_ then Expression'Not else id) (Expression'Match x0 x1 x2))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.match *> e2)
          <*> optional (Token.escape *> e2),
        Expression'NotEquals
          <$> e1
          <*> (choice [Token.exclamationMarkEqualsSign, Token.lessThanSignGreaterThanSign] *> e2),
        (\x0 not_ x1 x2 -> (if not_ then Expression'Not else id) (Expression'Regexp x0 x1 x2))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.regexp *> e2)
          <*> optional (Token.escape *> e2),
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

foreignKeyClauseParser :: forall r. Parser r ForeignKeyClause
foreignKeyClauseParser =
  (\reference (onDelete, onUpdate) deferred -> ForeignKeyClause {reference, onDelete, onUpdate, deferred})
    <$> (Token.references *> schemaQualified referenceParser)
    <*> (fromActions <$> many actionClauseParser)
    <*> deferredParser
  where
    fromActions :: [Maybe (Either Action Action)] -> (Action, Action)
    fromActions =
      foldr (\x (y, z) -> maybe (y, z) (either (,z) (y,)) x) (Action'NoAction, Action'NoAction)

    actionParser :: Parser r Action
    actionParser =
      choice
        [ Action'Cascade <$ Token.cascade,
          Action'NoAction <$ (Token.no *> Token.action),
          Action'Restrict <$ Token.restrict,
          Action'SetDefault <$ (Token.set *> Token.default_),
          Action'SetNull <$ (Token.set *> Token.null)
        ]

    actionClauseParser :: Parser r (Maybe (Either Action Action))
    actionClauseParser =
      choice
        [ Just . Left <$> (Token.on *> Token.delete *> actionParser),
          Just . Right <$> (Token.on *> Token.update *> actionParser),
          Nothing <$ (Token.match *> Token.identifier)
        ]

    deferredParser :: Parser r Bool
    deferredParser =
      choice
        [ f
            <$> perhaps Token.not
            <*> (Token.deferrable *> optional (Token.initially *> choice [Token.deferred, Token.immediate])),
          pure False
        ]
      where
        f :: Bool -> Maybe Token -> Bool
        f False (Just Token.DEFERRED) = True
        f _ _ = False

    referenceParser :: Parser r Reference
    referenceParser =
      Reference
        <$> Token.identifier
        <*> choice
          [ parens (NonEmpty.toList <$> commaSep1 Token.identifier),
            pure []
          ]

functionCallParser :: Parser r (f Expression) -> Parser r (FunctionCall f)
functionCallParser arguments =
  FunctionCall
    <$> schemaQualified Token.identifier
    <*> parens arguments

data GeneratedType
  = GeneratedType'Stored
  | GeneratedType'Virtual
  deriving stock (Eq, Generic, Show)

generatedType :: Parser r GeneratedType
generatedType =
  choice
    [ GeneratedType'Stored <$ Token.stored,
      GeneratedType'Virtual <$ Token.virtual
    ]

-- | https://sqlite.org/syntax/indexed-column.html
data IndexedColumn = IndexedColumn
  { column :: Either Text Expression,
    collation :: Maybe Text,
    ordering :: Ordering
  }
  deriving stock (Eq, Generic, Show)

makeIndexedColumn :: Parser r Expression -> Parser r IndexedColumn
makeIndexedColumn expression =
  IndexedColumn
    <$> choice
      [ Left <$> Token.identifier,
        Right <$> expression
      ]
    <*> optional (Token.collate *> Token.identifier)
    <*> orderingParser

literalValue :: Parser r LiteralValue
literalValue =
  choice
    [ LiteralValue'Blob <$> Token.blob,
      LiteralValue'Boolean False <$ Token.false,
      LiteralValue'Boolean True <$ Token.true,
      LiteralValue'CurrentDate <$ Token.currentDate,
      LiteralValue'CurrentTime <$ Token.currentTime,
      LiteralValue'CurrentTimestamp <$ Token.currentTimestamp,
      LiteralValue'Null <$ Token.null,
      LiteralValue'Number <$> Token.number,
      LiteralValue'String <$> Token.string
    ]

nullsWhichParser :: Parser r NullsWhich
nullsWhichParser =
  choice
    [ NullsWhich'First <$ (Token.nulls *> Token.first),
      NullsWhich'Last <$ (Token.nulls *> Token.last)
    ]

-- | https://sqlite.org/syntax/conflict-clause.html
data OnConflict
  = OnConflict'Abort
  | OnConflict'Fail
  | OnConflict'Ignore
  | OnConflict'Replace
  | OnConflict'Rollback
  deriving stock (Eq, Generic, Show)

onConflictParser :: Parser r OnConflict
onConflictParser =
  Token.on
    *> Token.conflict
    *> choice
      [ OnConflict'Abort <$ Token.abort,
        OnConflict'Fail <$ Token.fail,
        OnConflict'Ignore <$ Token.ignore,
        OnConflict'Replace <$ Token.replace,
        OnConflict'Rollback <$ Token.rollback
      ]

orderingParser :: Parser r Ordering
orderingParser =
  choice
    [ Ordering'Asc <$ perhaps Token.asc,
      Ordering'Desc <$ Token.desc
    ]

makeOrderingTermParser :: Parser r Expression -> Parser r OrderingTerm
makeOrderingTermParser expressionParser =
  OrderingTerm
    <$> expressionParser
    <*> optional (Token.collate *> Token.identifier)
    <*> orderingParser
    <*> optional nullsWhichParser

overClauseParser :: Parser r OverClause
overClauseParser =
  Token.over
    *> choice
      [ OverClause'WindowDefinition <$> windowDefinitionParser,
        OverClause'WindowName <$> Token.identifier
      ]

raiseParser :: Parser r Raise
raiseParser =
  Token.raise *> parens (choice xs)
  where
    xs =
      [ Raise'Abort <$> errorMessage Token.abort,
        Raise'Fail <$> errorMessage Token.fail,
        Raise'Ignore <$ Token.ignore,
        Raise'Rollback <$> errorMessage Token.rollback
      ]
    errorMessage p =
      p *> Token.comma *> Token.string

-- | https://sqlite.org/syntax/returning-clause.html
newtype ReturningClause
  = ReturningClause (NonEmpty ReturningClauseItem)
  deriving stock (Eq, Generic, Show)

makeReturningClause :: Parser r ReturningClauseItem -> Parser r ReturningClause
makeReturningClause = undefined

data ReturningClauseItem
  = ReturningClauseItem'All
  | ReturningClauseItem'Expression (Aliased Maybe Expression)
  deriving stock (Eq, Generic, Show)

makeReturningClauseItem :: Parser r Expression -> Parser r ReturningClauseItem
makeReturningClauseItem = undefined

schemaQualified :: Parser r a -> Parser r (SchemaQualified a)
schemaQualified p =
  SchemaQualified
    <$> optional (Token.identifier <* Token.fullStop)
    <*> p

makeSelectStatementParser ::
  forall r.
  Parser r Expression ->
  Parser r QualifiedTableName ->
  Earley.Grammar r (Parser r SelectStatement)
makeSelectStatementParser expressionParser qualifiedTableNameParser = mdo
  commonTableExpressionParser <- Earley.rule (makeCommonTableExpressionParser selectStatementParser)
  compoundSelectParser <- makeCompoundSelectParser tableParser
  selectStatementParser <-
    Earley.rule do
      SelectStatement
        <$> optional (WithClause <$> (Token.with *> perhaps Token.recursive) <*> commaSep1 commonTableExpressionParser)
        <*> compoundSelectParser
        <*> optional (Token.order *> Token.by *> commaSep1 (makeOrderingTermParser expressionParser))
        <*> optional limitClauseParser
  tableParser <- makeTableParser selectStatementParser
  pure selectStatementParser
  where
    joinConstraintParser :: Parser r JoinConstraint
    joinConstraintParser =
      choice
        [ JoinConstraint'On <$> (Token.on *> expressionParser),
          JoinConstraint'Using <$> (Token.using *> parens (commaSep1 Token.identifier))
        ]

    limitClauseParser :: Parser r LimitClause
    limitClauseParser =
      munge
        <$> (Token.limit *> expressionParser)
        <*> optional
          ( choice
              [ Left <$> (Token.offset *> expressionParser),
                Right <$> (Token.comma *> expressionParser)
              ]
          )
      where
        munge :: Expression -> Maybe (Either Expression Expression) -> LimitClause
        munge e1 = \case
          -- LIMIT x
          Nothing -> LimitClause e1 Nothing
          -- LIMIT x OFFSET y
          Just (Left e2) -> LimitClause e1 (Just e2)
          -- LIMIT y, x
          Just (Right e2) -> LimitClause e2 (Just e1)

    makeCommonTableExpressionParser :: Parser r SelectStatement -> Parser r CommonTableExpression
    makeCommonTableExpressionParser selectStatementParser =
      CommonTableExpression
        <$> Token.identifier
        <*> optional (parens (commaSep1 Token.identifier))
        <*> ( Token.as
                *> optional
                  ( choice
                      [ False <$ (Token.not *> Token.materialized),
                        True <$ Token.materialized
                      ]
                  )
            )
        <*> parens selectStatementParser

    makeCompoundSelectParser :: Parser r Table -> Earley.Grammar r (Parser r CompoundSelect)
    makeCompoundSelectParser tableParser = mdo
      compoundSelectParser <-
        Earley.rule do
          choice
            [ CompoundSelect <$> selectCoreParser,
              CompoundSelect'Except <$> compoundSelectParser <*> (Token.except *> selectCoreParser),
              CompoundSelect'Intersect <$> compoundSelectParser <*> (Token.intersect *> selectCoreParser),
              CompoundSelect'Union <$> compoundSelectParser <*> (Token.union *> selectCoreParser),
              CompoundSelect'UnionAll <$> compoundSelectParser <*> (Token.union *> Token.all *> selectCoreParser)
            ]
      selectParser <-
        Earley.rule do
          Select
            <$> ( Token.select
                    *> choice
                      [ True <$ Token.distinct,
                        False <$ Token.all,
                        pure False
                      ]
                )
            <*> commaSep1 (schemaQualified (tableQualified Token.identifier))
            <*> optional (Token.from *> tableParser)
            <*> optional (Token.where_ *> expressionParser)
            <*> optional
              ( GroupByClause
                  <$> (Token.group *> Token.by *> commaSep1 expressionParser)
                  <*> optional (Token.having *> expressionParser)
              )
            <*> optional windowClauseParser
      selectCoreParser <-
        Earley.rule do
          choice
            [ SelectCore'Select <$> selectParser,
              SelectCore'Values <$> valuesParser
            ]
      valuesParser <- Earley.rule (Token.values *> commaSep1 (parens (commaSep1 expressionParser)))
      pure compoundSelectParser
      where
        windowClauseParser :: Parser r (NonEmpty (Aliased Identity WindowDefinition))
        windowClauseParser =
          Token.window
            *> commaSep1
              ( (\x y -> Aliased y (Identity x))
                  <$> Token.identifier
                  <*> (Token.as *> windowDefinitionParser)
              )

    makeTableParser :: Parser r SelectStatement -> Earley.Grammar r (Parser r Table)
    makeTableParser selectStatementParser = mdo
      tableParser <-
        Earley.rule do
          choice
            [ Table'CrossJoin
                <$> tableParser
                <*> (Token.cross *> Token.join *> tableParser0)
                <*> optional joinConstraintParser,
              Table'InnerJoin
                <$> tableParser
                <*> (Token.comma *> tableParser0)
                <*> pure Nothing,
              Table'InnerJoin
                <$> tableParser
                <*> (perhaps Token.inner *> Token.join *> tableParser0)
                <*> optional joinConstraintParser,
              Table'LeftOuterJoin
                <$> tableParser
                <*> (Token.left *> perhaps Token.outer *> Token.join *> tableParser0)
                <*> optional joinConstraintParser,
              Table'NaturalCrossJoin
                <$> tableParser
                <*> (Token.natural *> Token.cross *> Token.join *> tableParser0),
              Table'NaturalInnerJoin
                <$> tableParser
                <*> (Token.natural *> perhaps Token.inner *> Token.join *> tableParser0),
              Table'NaturalLeftOuterJoin
                <$> tableParser
                <*> (Token.natural *> Token.left *> perhaps Token.outer *> Token.join *> tableParser0),
              tableParser0
            ]
      tableParser0 <-
        Earley.rule do
          choice
            [ Table <$> qualifiedTableNameParser,
              Table'Function
                <$> ( Aliased
                        <$> functionCallParser (commaSep1 expressionParser)
                        <*> optional (Token.as *> Token.identifier)
                    ),
              Table'Subquery <$> (Aliased <$> parens selectStatementParser <*> optional (Token.as *> Token.identifier)),
              parens tableParser
            ]
      pure tableParser

data Sign
  = Sign'HyphenMinus
  | Sign'PlusSign
  deriving stock (Eq, Generic, Show)

sign :: Parser r Sign
sign =
  choice
    [ Sign'HyphenMinus <$ Token.hyphenMinus,
      Sign'PlusSign <$ Token.plusSign
    ]

-- | https://sqlite.org/syntax/signed-number.html
data SignedNumber
  = SignedNumber (Maybe Sign) Text
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Eq, Generic, Show)

data TableConstraint
  = TableConstraint'Check Expression
  | TableConstraint'ForeignKey (NonEmpty Text) ForeignKeyClause
  | TableConstraint'PrimaryKey (NonEmpty IndexedColumn) (Maybe OnConflict)
  | TableConstraint'Unique (NonEmpty IndexedColumn) (Maybe OnConflict)
  deriving stock (Eq, Generic, Show)

data TableDefinition = TableDefinition
  { columns :: NonEmpty ColumnDefinition,
    constraints :: [Named TableConstraint],
    withoutRowid :: Bool
  }
  deriving stock (Eq, Generic, Show)

tableQualified :: Parser r a -> Parser r (TableQualified a)
tableQualified p =
  TableQualified
    <$> optional (Token.identifier <* Token.fullStop)
    <*> p

data TransactionType
  = TransactionType'Deferred
  | TransactionType'Exclusive
  | TransactionType'Immediate
  deriving stock (Eq, Generic, Show)

windowDefinitionParser :: Parser r WindowDefinition
windowDefinitionParser = undefined
