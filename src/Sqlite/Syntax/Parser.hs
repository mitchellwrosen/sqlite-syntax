-- TODO export list
-- TODO move types out
module Sqlite.Syntax.Parser where

import Control.Applicative (many, optional)
import Control.Applicative.Combinators (choice)
import Control.Applicative.Combinators.NonEmpty (some)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Sqlite.Syntax.Internal.Parser.Rule.CommonTableExpression (makeCommonTableExpressionsRule)
import Sqlite.Syntax.Internal.Parser.Rule.ForeignKeyClause (foreignKeyClauseRule)
import Sqlite.Syntax.Internal.Parser.Rule.FunctionCall (functionCallRule)
import Sqlite.Syntax.Internal.Parser.Rule.IndexedColumn (indexedColumnRule)
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Rule.Ordering (orderingRule)
import Sqlite.Syntax.Internal.Parser.Rule.OrderingTerm (makeOrderingTermRule)
import Sqlite.Syntax.Internal.Parser.Rule.Table (makeTableRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Internal.Type.Aliased
import Sqlite.Syntax.Internal.Type.Expression
import Sqlite.Syntax.Internal.Type.ForeignKeyClause
import Sqlite.Syntax.Internal.Type.IndexedColumn
import Sqlite.Syntax.Internal.Type.LiteralValue
import Sqlite.Syntax.Internal.Type.Named
import Sqlite.Syntax.Internal.Type.Namespaced
import Sqlite.Syntax.Internal.Type.Ordering
import Sqlite.Syntax.Internal.Type.OrderingTerm
import Sqlite.Syntax.Internal.Type.SelectStatement
import Sqlite.Syntax.Internal.Type.Window
import Sqlite.Syntax.Lexer (lex)
import qualified Sqlite.Syntax.Parser.Token as Token
import Sqlite.Syntax.Token (LocatedToken (..))
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
  | ParseError Text (Maybe Int) (Set Text)
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
                (Set.fromList expected)
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
    renderError ("Expected " <> Text.intercalate ", " (Set.toList expected)) input maybeOffset
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
  commonTableExpressionsRule <- Earley.rule (makeCommonTableExpressionsRule selectStatementRule)
  expressionRule <- makeExpressionRule selectStatementRule windowParser
  orderingTermParser <- Earley.rule (makeOrderingTermRule expressionRule)
  selectStatementRule <-
    makeSelectStatementParser
      commonTableExpressionsRule
      expressionRule
      orderingTermParser
      tableRule
      windowParser
  tableRule <- makeTableRule expressionRule selectStatementRule
  windowParser <- Earley.rule (makeWindowParser expressionRule orderingTermParser)

  let columnDefinitionRule :: Rule r ColumnDefinition
      columnDefinitionRule =
        makeColumnDefinitionRule expressionRule

      createIndexStatement :: Rule r CreateIndexStatement
      createIndexStatement =
        makeCreateIndexStatement expressionRule

      createTableStatement :: Rule r CreateTableStatement
      createTableStatement =
        makeCreateTableStatement
          columnDefinitionRule
          expressionRule
          selectStatementRule

      statementParser :: Rule r Statement
      statementParser =
        choice
          [ Statement'AlterTable <$> makeAlterTableStatement columnDefinitionRule,
            Statement'Analyze <$> analyzeStatement,
            Statement'Attach <$> makeAttachStatement expressionRule,
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
            Statement'Select <$> selectStatementRule,
            Statement'Update <$> pure TODO,
            Statement'Vacuum <$> pure TODO
          ]
          <* perhaps_ Token.semicolon

  pure
    Syntax
      { synExpression = expressionRule,
        synStatement = statementParser
      }
  where
    makeWindowParser :: Rule r Expression -> Rule r OrderingTerm -> Rule r Window
    makeWindowParser expressionRule orderingTermParser =
      parens do
        Window
          <$> optional Token.identifier
          <*> optional (Token.partition *> Token.by *> commaSep1 expressionRule)
          <*> optional (Token.order *> Token.by *> commaSep1 orderingTermParser)
          <*> choice [frameParser, pure defaultFrame]
      where
        defaultFrame :: Frame
        defaultFrame =
          Frame Range (Preceding Nothing) CurrentRow ExcludeNoOthers

        frameParser :: Rule r Frame
        frameParser =
          ( \type_ (startingBoundary, endingBoundary) exclude ->
              Frame {type_, startingBoundary, endingBoundary, exclude}
          )
            <$> choice
              [ Groups <$ Token.groups,
                Range <$ Token.range,
                Rows <$ Token.rows
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
                  (,CurrentRow) <$> currentRowParser,
                  (,CurrentRow) <$> unboundedPrecedingParser,
                  (,CurrentRow) <$> exprPrecedingParser Just
                ]
              where
                currentRowParser :: Rule r (FrameBoundary f g)
                currentRowParser =
                  CurrentRow <$ (Token.current *> Token.row)

                exprFollowingParser :: (Expression -> f Expression) -> Rule r (FrameBoundary f g)
                exprFollowingParser f =
                  (Following . f) <$> (expressionRule <* Token.following)

                exprPrecedingParser :: (Expression -> g Expression) -> Rule r (FrameBoundary f g)
                exprPrecedingParser f =
                  (Preceding . f) <$> (expressionRule <* Token.preceding)

                unboundedFollowingParser :: Rule r (FrameBoundary Maybe g)
                unboundedFollowingParser =
                  Following Nothing <$ (Token.unbounded *> Token.following)

                unboundedPrecedingParser :: Rule r (FrameBoundary f Maybe)
                unboundedPrecedingParser =
                  Preceding Nothing <$ (Token.unbounded *> Token.preceding)

            frameExcludeParser :: Rule r FrameExclude
            frameExcludeParser =
              choice
                [ ExcludeCurrentRow <$ (Token.exclude *> Token.current *> Token.row),
                  ExcludeGroup <$ (Token.exclude *> Token.group),
                  ExcludeNoOthers <$ (Token.exclude *> Token.no *> Token.others),
                  ExcludeTies <$ (Token.exclude *> Token.ties),
                  pure ExcludeNoOthers
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
    <$> (Token.alter *> Token.table *> namespacedRule Token.identifier Token.identifier)
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
  AnalyzeStatement <$> (Token.analyze *> optional (namespacedRule Token.identifier Token.identifier))

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

makeColumnDefinitionRule :: forall r. Rule r Expression -> Rule r ColumnDefinition
makeColumnDefinitionRule expressionRule =
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
            [ ColumnConstraint'Check <$> (Token.check *> parens expressionRule),
              ColumnConstraint'Collate <$> (Token.collate *> Token.identifier),
              ColumnConstraint'Default
                <$> choice
                  [ Default'Expression <$> parens expressionRule,
                    Default'LiteralValue <$> literalValueRule,
                    Default'SignedNumber <$> signedNumber
                  ],
              ColumnConstraint'ForeignKey <$> foreignKeyClauseRule,
              ColumnConstraint'Generated
                <$> (optional (Token.generated *> Token.always) *> Token.as *> parens expressionRule)
                <*> optional generatedType,
              ColumnConstraint'NotNull <$> (Token.not *> Token.null *> optional onConflictParser),
              ColumnConstraint'PrimaryKey
                <$> (Token.primary *> Token.key *> orderingRule)
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

makeCreateIndexStatement :: Rule r Expression -> Rule r CreateIndexStatement
makeCreateIndexStatement expression =
  CreateIndexStatement
    <$> (Token.create *> perhaps Token.unique)
    <*> (Token.index *> perhaps (Token.if_ *> Token.not *> Token.exists))
    <*> namespacedRule Token.identifier Token.identifier
    <*> (Token.on *> Token.identifier)
    <*> parens (commaSep1 indexedColumnRule)
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
  Rule r SelectStatement ->
  Rule r CreateTableStatement
makeCreateTableStatement columnDefinition expression selectStatement =
  CreateTableStatement
    <$> (Token.create *> perhaps (choice [Token.temp, Token.temporary]))
    <*> (Token.table *> perhaps (Token.if_ *> Token.not *> Token.exists))
    <*> namespacedRule Token.identifier Token.identifier
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
                    <*> foreignKeyClauseRule,
                  TableConstraint'PrimaryKey
                    <$> (Token.primary *> Token.key *> parens (commaSep1 indexedColumnRule))
                    <*> optional onConflictParser,
                  TableConstraint'Unique
                    <$> (Token.unique *> parens (commaSep1 indexedColumnRule))
                    <*> optional onConflictParser
                ]

data Default
  = Default'Expression Expression
  | Default'LiteralValue LiteralValue
  | Default'SignedNumber SignedNumber
  deriving stock (Eq, Generic, Show)

makeExpressionRule :: Rule r SelectStatement -> Rule r Window -> Earley.Grammar r (Rule r Expression)
makeExpressionRule selectStatement windowParser = mdo
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
                    <$> functionCallRule (Identity <$> (Token.distinct *> expression))
                    <*> optional filterClauseParser
                ),
          Expression'Case <$> caseExpressionParser expression,
          Expression'Cast
            <$> ( CastExpression
                    <$> (Token.cast *> Token.leftParenthesis *> expression)
                    <*> (Token.as *> Token.identifier)
                ),
          Expression'Column <$> namespacedRule (namespacedRule Token.identifier Token.identifier) Token.identifier,
          Expression'Exists <$> (Token.exists *> parens selectStatement),
          Expression'FunctionCall
            <$> ( FunctionCallExpression
                    <$> functionCallRule
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
          <*> (Token.in_ *> functionCallRule (commaSep0 e0)),
        (\x0 not_ x1 -> (if not_ then Expression'Not else id) (Expression'InSubquery (InSubqueryExpression x0 x1)))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> parens selectStatement),
        (\x0 not_ x1 -> (if not_ then Expression'Not else id) (Expression'InTable (InTableExpression x0 x1)))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> namespacedRule Token.identifier Token.identifier),
        (\x0 not_ x1 -> (if not_ then Expression'Not else id) (Expression'InValues (InValuesExpression x0 x1)))
          <$> e1
          <*> perhaps Token.not
          <*> (Token.in_ *> parens (commaSep0 e0)),
        (\x0 not_ x1 -> (if not_ then Expression'Not else id) (Expression'Is x0 x1))
          <$> e1
          <*> (Token.is *> perhaps Token.not)
          <*> e2,
        (\x0 -> Expression'Is x0 (Expression'LiteralValue Null)) <$> e1 <* Token.isnull,
        (\x0 -> Expression'Not (Expression'Is x0 (Expression'LiteralValue Null)))
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
    caseExpressionParser expressionRule =
      CaseExpression
        <$> (Token.case_ *> optional expressionRule)
        <*> some ((,) <$> (Token.when *> expressionRule) <*> (Token.then_ *> expressionRule))
        <*> choice
          [ Token.else_ *> expressionRule,
            pure (Expression'LiteralValue Null)
          ]
        <* Token.end

    parameterParser :: Rule r Parameter
    parameterParser =
      choice
        [ Parameter'Named <$> Token.namedParameter,
          Parameter'Ordinal <$> Token.parameter
        ]

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

literalValueRule :: Rule r LiteralValue
literalValueRule =
  choice
    [ Blob <$> Token.blob,
      Boolean False <$ Token.false,
      Boolean True <$ Token.true,
      CurrentDate <$ Token.currentDate,
      CurrentTime <$ Token.currentTime,
      CurrentTimestamp <$ Token.currentTimestamp,
      Null <$ Token.null,
      Number <$> Token.number,
      String <$> Token.string
    ]

-- | https://sqlite.org/syntax/conflict-clause.html
data OnConflict
  = OnConflictAbort
  | OnConflictFail
  | OnConflictIgnore
  | OnConflictReplace
  | OnConflictRollback
  deriving stock (Eq, Generic, Show)

onConflictParser :: Rule r OnConflict
onConflictParser =
  Token.on
    *> Token.conflict
    *> choice
      [ OnConflictAbort <$ Token.abort,
        OnConflictFail <$ Token.fail,
        OnConflictIgnore <$ Token.ignore,
        OnConflictReplace <$ Token.replace,
        OnConflictRollback <$ Token.rollback
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
  Rule r CommonTableExpressions ->
  Rule r Expression ->
  Rule r OrderingTerm ->
  Rule r Table ->
  Rule r Window ->
  Earley.Grammar r (Rule r SelectStatement)
makeSelectStatementParser commonTableExpressionsRule expressionRule orderingTermParser tableRule windowParser = mdo
  compoundSelectParser <- makeCompoundSelectParser tableRule
  selectStatementRule <-
    Earley.rule do
      SelectStatement
        <$> optional commonTableExpressionsRule
        <*> compoundSelectParser
        <*> optional (Token.order *> Token.by *> commaSep1 orderingTermParser)
        <*> optional limitRule
  pure selectStatementRule
  where
    limitRule :: Rule r Limit
    limitRule =
      munge
        <$> (Token.limit *> expressionRule)
        <*> optional
          ( choice
              [ Left <$> (Token.offset *> expressionRule),
                Right <$> (Token.comma *> expressionRule)
              ]
          )
      where
        munge :: Expression -> Maybe (Either Expression Expression) -> Limit
        munge e1 = \case
          -- LIMIT x
          Nothing -> Limit e1 Nothing
          -- LIMIT x OFFSET y
          Just (Left e2) -> Limit e1 (Just e2)
          -- LIMIT y, x
          Just (Right e2) -> Limit e2 (Just e1)

    makeCompoundSelectParser :: Rule r Table -> Earley.Grammar r (Rule r CompoundSelect)
    makeCompoundSelectParser tableParser = mdo
      compoundSelectParser <-
        Earley.rule do
          choice
            [ CompoundSelect <$> selectCoreParser,
              Except <$> compoundSelectParser <*> (Token.except *> selectCoreParser),
              Intersect <$> compoundSelectParser <*> (Token.intersect *> selectCoreParser),
              Union <$> compoundSelectParser <*> (Token.union *> selectCoreParser),
              UnionAll <$> compoundSelectParser <*> (Token.union *> Token.all *> selectCoreParser)
            ]
      selectParser <-
        Earley.rule do
          Select
            <$> distinctParser
            <*> commaSep1 resultColumnRule
            <*> optional (Token.from *> tableParser)
            <*> optional (Token.where_ *> expressionRule)
            <*> optional groupByRule
            <*> optional windowClauseParser
      selectCoreParser <-
        Earley.rule do
          choice
            [ SelectCore'Select <$> selectParser,
              SelectCore'Values <$> valuesParser
            ]
      valuesParser <- Earley.rule (Token.values *> commaSep1 (parens (commaSep1 expressionRule)))
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
        groupByRule :: Rule r GroupBy
        groupByRule =
          GroupBy
            <$> (Token.group *> Token.by *> commaSep1 expressionRule)
            <*> optional (Token.having *> expressionRule)
        resultColumnRule :: Rule r ResultColumn
        resultColumnRule =
          choice
            [ ResultColumn'Expression <$> (Aliased <$> expressionRule <*> optional (Token.as *> Token.identifier)),
              ResultColumn'Wildcard <$> namespacedRule Token.identifier (() <$ Token.asterisk)
            ]
        windowClauseParser :: Rule r (NonEmpty (Aliased Identity Window))
        windowClauseParser =
          Token.window
            *> commaSep1
              ( (\x y -> Aliased y (Identity x))
                  <$> Token.identifier
                  <*> (Token.as *> windowParser)
              )

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
