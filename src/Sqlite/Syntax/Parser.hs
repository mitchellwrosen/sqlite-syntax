-- TODO export list
-- TODO move types out
module Sqlite.Syntax.Parser where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Control.Applicative.Combinators.NonEmpty (some)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Aliased
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.ForeignKeyClause
import Sqlite.Syntax.Internal.Type.FunctionCall
import Sqlite.Syntax.Internal.Type.LiteralValue
import Sqlite.Syntax.Internal.Type.Named
import Sqlite.Syntax.Internal.Type.Namespaced
import Sqlite.Syntax.Internal.Type.OrderingTerm
import Sqlite.Syntax.Internal.Type.QualifiedTableName
import Sqlite.Syntax.Internal.Type.SelectStatement
import Sqlite.Syntax.Internal.Type.Window
import Sqlite.Syntax.Lexer (lex)
import qualified Sqlite.Syntax.Parser.Token as Token
import Sqlite.Syntax.Token (LocatedToken (..), Token)
import qualified Sqlite.Syntax.Token as Token
import qualified Text.Earley as Earley
import qualified Text.Megaparsec as Megaparsec
import Prelude hiding (Ordering, fail, lex, not, null)

-- TODO simplify some things with defaults (e.g. missing distinct/all == all)
-- TODO move signed-number to lexer probably
-- TODO name things *Parser
-- TODO does 'many' need to be 'rule'd?
-- TODO eliminate Maybes by using the defaults
-- TODO make sure all of the make* things called multiple times aren't involved in mutual recursion
-- TODO rm *Clause
-- TODO Parser -> Rule
-- TODO quirks page says many keywords are valid identifiers..

--

-- TODO this is temporary
data ParseError
  = SyntaxError Text Int
  | ParseError Text (Maybe Int) [Text]
  | -- Temporary
    forall a. Show a => AmbiguousParse a

parse :: Show a => Earley.Parser Text [LocatedToken] a -> Text -> Either ParseError a
parse parser sql =
  case lex sql of
    Left errorBundle ->
      Left (SyntaxError sql (Megaparsec.errorOffset (NonEmpty.head (Megaparsec.bundleErrors errorBundle))))
    Right tokens ->
      case Earley.fullParses parser tokens of
        ([], Earley.Report {expected, unconsumed}) ->
          Left
            ( ParseError
                sql
                ( case unconsumed of
                    [] -> Nothing
                    LocatedToken _ offset : _ -> Just offset
                )
                expected
            )
        ([s], _) -> Right s
        (ss, _) -> Left (AmbiguousParse ss)

debugParseFile :: FilePath -> IO ()
debugParseFile path = do
  sql <- Text.readFile ("test/files/" ++ path)
  case parseStatement sql of
    Left err -> Text.putStrLn (renderParseError err)
    Right statement -> print statement

renderParseError :: ParseError -> Text
renderParseError = \case
  SyntaxError input offset -> renderError "Syntax error" input (Just offset)
  ParseError input maybeOffset expected ->
    renderError ("Expected " <> Text.intercalate ", " expected) input maybeOffset
  AmbiguousParse ss -> Text.pack (show ss)
  where
    renderError :: Text -> Text -> Maybe Int -> Text
    renderError message code maybeOffset =
      fromMaybe message do
        offset <- maybeOffset
        context <- maybeContext offset
        pure (message <> "\n\n" <> context)
      where
        maybeContext :: Int -> Maybe Text
        maybeContext offset = do
          let ( maybeInputLine,
                Megaparsec.PosState {pstateSourcePos = Megaparsec.SourcePos {sourceColumn, sourceLine}}
                ) =
                  Megaparsec.reachOffset offset initialState
          inputLine <- maybeInputLine
          let line = Text.pack (show (Megaparsec.unPos sourceLine))
              left = Text.replicate (Text.length line) " " <> " │"
          pure do
            Text.unlines
              [ left,
                line <> " │ " <> Text.pack inputLine,
                left <> Text.replicate (Megaparsec.unPos sourceColumn) " " <> "↑"
              ]
          where
            initialState :: Megaparsec.PosState Text
            initialState =
              Megaparsec.PosState
                { pstateInput = code,
                  pstateOffset = 0,
                  pstateSourcePos = Megaparsec.initialPos "",
                  pstateTabWidth = Megaparsec.defaultTabWidth,
                  pstateLinePrefix = ""
                }

parseExpression :: Text -> Either ParseError Expression
parseExpression =
  parse (Earley.parser (synExpression <$> syntaxParser))

parseStatement :: Text -> Either ParseError Statement
parseStatement =
  parse (Earley.parser (synStatement <$> syntaxParser))

--

data Syntax r = Syntax
  { synExpression :: Rule r Expression,
    synStatement :: Rule r Statement
  }

syntaxParser :: forall r. Earley.Grammar r (Syntax r)
syntaxParser = mdo
  columnDefinitionParser <- Earley.rule (makeColumnDefinitionParser expressionParser)
  expressionParser <- makeExpression selectStatementParser windowParser
  indexedColumnParser <- Earley.rule (makeIndexedColumn expressionParser)
  orderingTermParser <- Earley.rule (makeOrderingTermParser expressionParser)
  selectStatementParser <-
    makeSelectStatementParser
      expressionParser
      orderingTermParser
      windowParser
  windowParser <- Earley.rule (makeWindowParser expressionParser orderingTermParser)

  let createIndexStatement :: Rule r CreateIndexStatement
      createIndexStatement =
        makeCreateIndexStatement expressionParser indexedColumnParser

      createTableStatement :: Rule r CreateTableStatement
      createTableStatement =
        makeCreateTableStatement
          columnDefinitionParser
          expressionParser
          indexedColumnParser
          selectStatementParser

      statementParser :: Rule r Statement
      statementParser =
        choice
          [ Statement'AlterTable <$> makeAlterTableStatement columnDefinitionParser,
            Statement'Analyze <$> analyzeStatement,
            Statement'Attach <$> makeAttachStatement expressionParser,
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
          <* perhaps_ Token.semicolon

  pure
    Syntax
      { synExpression = expressionParser,
        synStatement = statementParser
      }
  where
    makeOrderingTermParser :: Rule r Expression -> Rule r OrderingTerm
    makeOrderingTermParser expressionParser =
      OrderingTerm
        <$> expressionParser
        <*> optional (Token.collate *> Token.identifier)
        <*> orderingParser
        <*> optional nullsWhichParser

    makeWindowParser :: Rule r Expression -> Rule r OrderingTerm -> Rule r Window
    makeWindowParser expressionParser orderingTermParser =
      parens do
        Window
          <$> optional Token.identifier
          <*> optional (Token.partition *> Token.by *> commaSep1 expressionParser)
          <*> optional (Token.order *> Token.by *> commaSep1 orderingTermParser)
          <*> choice [frameParser, pure defaultFrame]
      where
        defaultFrame :: Frame
        defaultFrame =
          Frame FrameType'Range (FrameBoundary'Preceding Nothing) FrameBoundary'CurrentRow FrameExclude'NoOthers

        frameParser :: Rule r Frame
        frameParser =
          ( \type_ (startingBoundary, endingBoundary) exclude ->
              Frame {type_, startingBoundary, endingBoundary, exclude}
          )
            <$> choice
              [ FrameType'Groups <$ Token.groups,
                FrameType'Range <$ Token.range,
                FrameType'Rows <$ Token.rows
              ]
            <*> frameBoundariesParser
            <*> frameExcludeParser
          where
            frameBoundariesParser :: Rule r (FrameBoundary Identity Maybe, FrameBoundary Maybe Identity)
            frameBoundariesParser =
              choice
                [ (,)
                    <$> ( Token.between
                            *> choice
                              [ currentRowParser,
                                exprFollowingParser Identity,
                                unboundedPrecedingParser,
                                exprPrecedingParser Just
                              ]
                        )
                    <*> ( Token.and
                            *> choice
                              [ currentRowParser,
                                exprFollowingParser Just,
                                unboundedFollowingParser,
                                exprPrecedingParser Identity
                              ]
                        ),
                  (,FrameBoundary'CurrentRow) <$> currentRowParser,
                  (,FrameBoundary'CurrentRow) <$> unboundedPrecedingParser,
                  (,FrameBoundary'CurrentRow) <$> exprPrecedingParser Just
                ]
              where
                currentRowParser :: Rule r (FrameBoundary f g)
                currentRowParser =
                  FrameBoundary'CurrentRow <$ (Token.current *> Token.row)

                exprFollowingParser :: (Expression -> f Expression) -> Rule r (FrameBoundary f g)
                exprFollowingParser f =
                  (FrameBoundary'Following . f) <$> (expressionParser <* Token.following)

                exprPrecedingParser :: (Expression -> g Expression) -> Rule r (FrameBoundary f g)
                exprPrecedingParser f =
                  (FrameBoundary'Preceding . f) <$> (expressionParser <* Token.preceding)

                unboundedFollowingParser :: Rule r (FrameBoundary Maybe g)
                unboundedFollowingParser =
                  FrameBoundary'Following Nothing <$ (Token.unbounded *> Token.following)

                unboundedPrecedingParser :: Rule r (FrameBoundary f Maybe)
                unboundedPrecedingParser =
                  FrameBoundary'Preceding Nothing <$ (Token.unbounded *> Token.preceding)

            frameExcludeParser :: Rule r FrameExclude
            frameExcludeParser =
              choice
                [ FrameExclude'CurrentRow <$ (Token.exclude *> Token.current *> Token.row),
                  FrameExclude'Group <$ (Token.exclude *> Token.group),
                  FrameExclude'NoOthers <$ (Token.exclude *> Token.no *> Token.others),
                  FrameExclude'Ties <$ (Token.exclude *> Token.ties),
                  pure FrameExclude'NoOthers
                ]

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

--

-- | https://sqlite.org/lang_altertable.html
data AlterTableStatement = AlterTableStatement
  { table :: Namespaced Text Text,
    alteration :: TableAlteration
  }
  deriving stock (Eq, Generic, Show)

makeAlterTableStatement :: Rule r ColumnDefinition -> Rule r AlterTableStatement
makeAlterTableStatement columnDefinition =
  AlterTableStatement
    <$> (Token.alter *> Token.table *> namespaced Token.identifier Token.identifier)
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
  = AnalyzeStatement (Maybe (Namespaced Text Text))
  deriving stock (Eq, Generic, Show)

analyzeStatement :: Rule r AnalyzeStatement
analyzeStatement =
  AnalyzeStatement <$> (Token.analyze *> optional (namespaced Token.identifier Token.identifier))

-- | https://sqlite.org/syntax/attach-stmt.html
data AttachStatement = AttachStatement
  { database :: Expression,
    schema :: Text
  }
  deriving stock (Eq, Generic, Show)

makeAttachStatement :: Rule r Expression -> Rule r AttachStatement
makeAttachStatement expression =
  AttachStatement <$> (Token.attach *> optional Token.database *> expression) <*> (Token.as *> Token.identifier)

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

makeColumnDefinitionParser :: forall r. Rule r Expression -> Rule r ColumnDefinition
makeColumnDefinitionParser expressionParser =
  ColumnDefinition
    <$> Token.identifier
    <*> optional Token.identifier
    <*> many namedColumnConstraintParser
  where
    namedColumnConstraintParser :: Rule r (Named ColumnConstraint)
    namedColumnConstraintParser =
      Named
        <$> optional (Token.constraint *> Token.identifier)
        <*> columnConstraintParser
      where
        columnConstraintParser :: Rule r ColumnConstraint
        columnConstraintParser =
          choice
            [ ColumnConstraint'Check <$> (Token.check *> parens expressionParser),
              ColumnConstraint'Collate <$> (Token.collate *> Token.identifier),
              ColumnConstraint'Default
                <$> choice
                  [ Default'Expression <$> parens expressionParser,
                    Default'LiteralValue <$> literalValueRule,
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
    name :: Namespaced Text Text,
    table :: Text,
    columns :: NonEmpty IndexedColumn,
    where_ :: Maybe Expression
  }
  deriving stock (Eq, Generic, Show)

makeCreateIndexStatement :: Rule r Expression -> Rule r IndexedColumn -> Rule r CreateIndexStatement
makeCreateIndexStatement expression indexedColumn =
  CreateIndexStatement
    <$> (Token.create *> perhaps Token.unique)
    <*> (Token.index *> perhaps (Token.if_ *> Token.not *> Token.exists))
    <*> namespaced Token.identifier Token.identifier
    <*> (Token.on *> Token.identifier)
    <*> parens (commaSep1 indexedColumn)
    <*> optional (Token.where_ *> expression)

-- | https://sqlite.org/syntax/create-table-stmt.html
data CreateTableStatement = CreateTableStatement
  { temporary :: Bool,
    ifNotExists :: Bool,
    name :: Namespaced Text Text,
    definition :: Either SelectStatement TableDefinition
  }
  deriving stock (Eq, Generic, Show)

makeCreateTableStatement ::
  forall r.
  Rule r ColumnDefinition ->
  Rule r Expression ->
  Rule r IndexedColumn ->
  Rule r SelectStatement ->
  Rule r CreateTableStatement
makeCreateTableStatement columnDefinition expression indexedColumn selectStatement =
  CreateTableStatement
    <$> (Token.create *> perhaps (choice [Token.temp, Token.temporary]))
    <*> (Token.table *> perhaps (Token.if_ *> Token.not *> Token.exists))
    <*> namespaced Token.identifier Token.identifier
    <*> choice
      [ Left <$> (Token.as *> selectStatement),
        Right <$> tableDefinitionParser
      ]
  where
    tableDefinitionParser :: Rule r TableDefinition
    tableDefinitionParser =
      TableDefinition
        <$> (Token.leftParenthesis *> commaSep1 columnDefinition)
        <*> many (Token.comma *> namedTableConstraintParser)
        <* Token.rightParenthesis
        <*> perhaps (Token.without *> Token.rowid)
      where
        namedTableConstraintParser :: Rule r (Named TableConstraint)
        namedTableConstraintParser =
          Named
            <$> optional (Token.constraint *> Token.identifier)
            <*> tableConstraintParser
          where
            tableConstraintParser :: Rule r TableConstraint
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

makeExpression :: Rule r SelectStatement -> Rule r Window -> Earley.Grammar r (Rule r Expression)
makeExpression selectStatement windowParser = mdo
  filterClauseParser <- Earley.rule (Token.filter *> parens (Token.where_ *> expression))

  expression <-
    Earley.rule do
      choice
        [ Expression'Not <$> (Token.not *> expression),
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
        [ Expression'AggregateDistinctFunctionCall
            <$> ( AggregateDistinctFunctionCallExpression
                    <$> functionCallParser (Identity <$> (Token.distinct *> expression))
                    <*> optional filterClauseParser
                ),
          Expression'Case <$> caseExpressionParser expression,
          Expression'Cast
            <$> ( CastExpression
                    <$> (Token.cast *> Token.leftParenthesis *> expression)
                    <*> (Token.as *> Token.identifier)
                ),
          Expression'Column <$> namespaced (namespaced Token.identifier Token.identifier) Token.identifier,
          Expression'Exists <$> (Token.exists *> parens selectStatement),
          Expression'FunctionCall
            <$> ( FunctionCallExpression
                    <$> functionCallParser
                      ( choice
                          [ FunctionArguments'Arguments <$> commaSep0 expression,
                            FunctionArguments'Wildcard <$ Token.asterisk
                          ]
                      )
                    <*> optional filterClauseParser
                    <*> optional
                      ( Token.over
                          *> choice
                            [ Over'Window <$> windowParser,
                              Over'WindowName <$> Token.identifier
                            ]
                      )
                ),
          Expression'LiteralValue <$> literalValueRule,
          Expression'Parameter <$> parameterParser,
          Expression'Raise <$> raiseParser,
          Expression'RowValue
            <$> ( RowValue
                    <$> (Token.leftParenthesis *> expression)
                    <*> (Token.comma *> expression)
                    <*> many (Token.comma *> expression)
                ),
          Expression'Subquery <$> parens selectStatement,
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
          <*> (Token.in_ *> namespaced Token.identifier Token.identifier),
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

    caseExpressionParser :: Rule r Expression -> Rule r CaseExpression
    caseExpressionParser expressionParser =
      CaseExpression
        <$> (Token.case_ *> optional expressionParser)
        <*> some ((,) <$> (Token.when *> expressionParser) <*> (Token.then_ *> expressionParser))
        <*> choice
          [ Token.else_ *> expressionParser,
            pure (Expression'LiteralValue LiteralValue'Null)
          ]
        <* Token.end

    parameterParser :: Rule r Parameter
    parameterParser =
      choice
        [ Parameter'Named <$> Token.namedParameter,
          Parameter'Ordinal <$> Token.parameter
        ]

foreignKeyClauseParser :: forall r. Rule r ForeignKeyClause
foreignKeyClauseParser =
  (\reference (onDelete, onUpdate) deferred -> ForeignKeyClause {reference, onDelete, onUpdate, deferred})
    <$> (Token.references *> namespaced Token.identifier referenceParser)
    <*> (fromActions <$> many actionClauseParser)
    <*> deferredParser
  where
    fromActions :: [Maybe (Either Action Action)] -> (Action, Action)
    fromActions =
      foldr (\x (y, z) -> maybe (y, z) (either (,z) (y,)) x) (Action'NoAction, Action'NoAction)

    actionParser :: Rule r Action
    actionParser =
      choice
        [ Action'Cascade <$ Token.cascade,
          Action'NoAction <$ (Token.no *> Token.action),
          Action'Restrict <$ Token.restrict,
          Action'SetDefault <$ (Token.set *> Token.default_),
          Action'SetNull <$ (Token.set *> Token.null)
        ]

    actionClauseParser :: Rule r (Maybe (Either Action Action))
    actionClauseParser =
      choice
        [ Just . Left <$> (Token.on *> Token.delete *> actionParser),
          Just . Right <$> (Token.on *> Token.update *> actionParser),
          Nothing <$ (Token.match *> Token.identifier)
        ]

    deferredParser :: Rule r Bool
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

    referenceParser :: Rule r Reference
    referenceParser =
      Reference
        <$> Token.identifier
        <*> choice
          [ parens (NonEmpty.toList <$> commaSep1 Token.identifier),
            pure []
          ]

functionCallParser :: Rule r (f Expression) -> Rule r (FunctionCall f)
functionCallParser arguments =
  FunctionCall
    <$> namespaced Token.identifier Token.identifier
    <*> parens arguments

data GeneratedType
  = GeneratedType'Stored
  | GeneratedType'Virtual
  deriving stock (Eq, Generic, Show)

generatedType :: Rule r GeneratedType
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

makeIndexedColumn :: Rule r Expression -> Rule r IndexedColumn
makeIndexedColumn expression =
  IndexedColumn
    <$> choice
      [ Left <$> Token.identifier,
        Right <$> expression
      ]
    <*> optional (Token.collate *> Token.identifier)
    <*> orderingParser

literalValueRule :: Rule r LiteralValue
literalValueRule =
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

namespaced :: Rule r a -> Rule r b -> Rule r (Namespaced a b)
namespaced p1 p2 =
  Namespaced
    <$> optional (p1 <* Token.fullStop)
    <*> p2

nullsWhichParser :: Rule r NullsWhich
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

onConflictParser :: Rule r OnConflict
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

orderingParser :: Rule r Ordering
orderingParser =
  choice
    [ Ordering'Asc <$ perhaps_ Token.asc,
      Ordering'Desc <$ Token.desc
    ]

raiseParser :: Rule r Raise
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

makeReturningClause :: Rule r ReturningClauseItem -> Rule r ReturningClause
makeReturningClause = undefined

data ReturningClauseItem
  = ReturningClauseItem'All
  | ReturningClauseItem'Expression (Aliased Maybe Expression)
  deriving stock (Eq, Generic, Show)

makeReturningClauseItem :: Rule r Expression -> Rule r ReturningClauseItem
makeReturningClauseItem = undefined

makeSelectStatementParser ::
  forall r.
  Rule r Expression ->
  Rule r OrderingTerm ->
  Rule r Window ->
  Earley.Grammar r (Rule r SelectStatement)
makeSelectStatementParser expressionParser orderingTermParser windowParser = mdo
  commonTableExpressionParser <- Earley.rule (makeCommonTableExpressionParser selectStatementParser)
  compoundSelectParser <- makeCompoundSelectParser tableParser
  selectStatementParser <-
    Earley.rule do
      SelectStatement
        <$> optional (WithClause <$> (Token.with *> perhaps Token.recursive) <*> commaSep1 commonTableExpressionParser)
        <*> compoundSelectParser
        <*> optional (Token.order *> Token.by *> commaSep1 orderingTermParser)
        <*> optional limitClauseParser
  tableParser <- makeTableParser selectStatementParser
  pure selectStatementParser
  where
    joinConstraintParser :: Rule r JoinConstraint
    joinConstraintParser =
      choice
        [ JoinConstraint'On <$> (Token.on *> expressionParser),
          JoinConstraint'Using <$> (Token.using *> parens (commaSep1 Token.identifier))
        ]

    limitClauseParser :: Rule r LimitClause
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

    makeCommonTableExpressionParser :: Rule r SelectStatement -> Rule r CommonTableExpression
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

    makeCompoundSelectParser :: Rule r Table -> Earley.Grammar r (Rule r CompoundSelect)
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
            <$> distinctParser
            <*> commaSep1 resultColumnRule
            <*> optional (Token.from *> tableParser)
            <*> optional (Token.where_ *> expressionParser)
            <*> optional groupByClauseParser
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
        distinctParser :: Rule r Bool
        distinctParser =
          Token.select
            *> choice
              [ True <$ Token.distinct,
                False <$ Token.all,
                pure False
              ]
        groupByClauseParser :: Rule r GroupByClause
        groupByClauseParser =
          GroupByClause
            <$> (Token.group *> Token.by *> commaSep1 expressionParser)
            <*> optional (Token.having *> expressionParser)
        resultColumnRule :: Rule r ResultColumn
        resultColumnRule =
          choice
            [ ResultColumn'Expression <$> (Aliased <$> expressionParser <*> optional (Token.as *> Token.identifier)),
              ResultColumn'Wildcard <$> namespaced Token.identifier (() <$ Token.asterisk)
            ]
        windowClauseParser :: Rule r (NonEmpty (Aliased Identity Window))
        windowClauseParser =
          Token.window
            *> commaSep1
              ( (\x y -> Aliased y (Identity x))
                  <$> Token.identifier
                  <*> (Token.as *> windowParser)
              )

    makeTableParser :: Rule r SelectStatement -> Earley.Grammar r (Rule r Table)
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
      where
        qualifiedTableNameParser :: Rule r QualifiedTableName
        qualifiedTableNameParser =
          QualifiedTableName
            <$> ( Aliased
                    <$> namespaced Token.identifier Token.identifier
                    <*> optional (Token.as *> Token.identifier)
                )
            <*> optional indexedByParser
          where
            indexedByParser :: Rule r IndexedBy
            indexedByParser =
              choice
                [ IndexedBy'IndexedBy <$> (Token.indexed *> Token.by *> Token.identifier),
                  IndexedBy'NotIndexed <$ (Token.not *> Token.indexed)
                ]

data Sign
  = Sign'HyphenMinus
  | Sign'PlusSign
  deriving stock (Eq, Generic, Show)

sign :: Rule r Sign
sign =
  choice
    [ Sign'HyphenMinus <$ Token.hyphenMinus,
      Sign'PlusSign <$ Token.plusSign
    ]

-- | https://sqlite.org/syntax/signed-number.html
data SignedNumber
  = SignedNumber (Maybe Sign) Text
  deriving stock (Eq, Generic, Show)

signedNumber :: Rule r SignedNumber
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

data TransactionType
  = TransactionType'Deferred
  | TransactionType'Exclusive
  | TransactionType'Immediate
  deriving stock (Eq, Generic, Show)
