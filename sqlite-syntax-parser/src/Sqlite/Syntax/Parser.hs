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
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Rule.CommonTableExpression (makeCommonTableExpressionsRule)
import Sqlite.Syntax.Internal.Parser.Rule.DeleteStatement (makeDeleteStatementRule)
import Sqlite.Syntax.Internal.Parser.Rule.ForeignKeyClause (foreignKeyClauseRule)
import Sqlite.Syntax.Internal.Parser.Rule.FunctionCall (functionCallRule)
import Sqlite.Syntax.Internal.Parser.Rule.IndexedColumn (indexedColumnRule)
import Sqlite.Syntax.Internal.Parser.Rule.Namespaced (namespacedRule)
import Sqlite.Syntax.Internal.Parser.Rule.Ordering (orderingRule)
import Sqlite.Syntax.Internal.Parser.Rule.OrderingTerm (makeOrderingTermRule)
import Sqlite.Syntax.Internal.Parser.Rule.Returning (makeReturningRule)
import Sqlite.Syntax.Internal.Parser.Rule.SelectStatement (makeCompoundSelectRule, makeSelectStatementRule)
import Sqlite.Syntax.Internal.Parser.Rule.Table (makeTableRule)
import Sqlite.Syntax.Internal.Parser.Rule.Values (makeValuesRule)
import Sqlite.Syntax.Internal.Parser.Utils
import Sqlite.Syntax.Lexer (lex)
import qualified Sqlite.Syntax.Parser.Token as Token
import Sqlite.Syntax.Token (LocatedToken (..))
import qualified Text.Earley as Earley
import qualified Text.Megaparsec as Megaparsec
import Prelude hiding (Ordering, fail, lex, not, null)

-- TODO simplify some things with defaults (e.g. missing distinct/all == all)
-- TODO move signed-number to lexer probably
-- TODO does 'many' need to be 'rule'd?
-- TODO eliminate Maybes by using the defaults
-- TODO make sure all of the make* things called multiple times aren't involved in mutual recursion
-- TODO rm *Clause
-- TODO quirks page says many keywords are valid identifiers..
-- TODO combine Aliased/Named?

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
  compoundSelectRule <- makeCompoundSelectRule expressionRule tableRule valuesRule windowRule
  expressionRule <- makeExpressionRule selectStatementRule windowRule
  orderingTermRule <- Earley.rule (makeOrderingTermRule expressionRule)
  selectStatementRule <-
    Earley.rule do
      makeSelectStatementRule commonTableExpressionsRule compoundSelectRule expressionRule orderingTermRule
  tableRule <- makeTableRule expressionRule selectStatementRule
  valuesRule <- Earley.rule (makeValuesRule expressionRule)
  windowRule <- Earley.rule (makeWindowRule expressionRule orderingTermRule)

  let columnDefinitionRule :: Rule r ColumnDefinition
      columnDefinitionRule =
        makeColumnDefinitionRule expressionRule

      createIndexStatement :: Rule r CreateIndexStatement
      createIndexStatement =
        makeCreateIndexStatement expressionRule

      createTableStatement :: Rule r CreateTableStatement
      createTableStatement =
        makeCreateTableStatement columnDefinitionRule expressionRule selectStatementRule

      deleteStatementRule :: Rule r DeleteStatement
      deleteStatementRule =
        makeDeleteStatementRule commonTableExpressionsRule expressionRule returningRule

      returningRule :: Rule r Returning
      returningRule =
        makeReturningRule expressionRule

      statementRule :: Rule r Statement
      statementRule =
        choice
          [ Statement'AlterTable <$> makeAlterTableStatement columnDefinitionRule,
            Statement'Analyze <$> analyzeStatement,
            Statement'Attach <$> makeAttachStatement expressionRule,
            Statement'Begin
              <$> ( Token.begin
                      *> choice
                        [ Exclusive <$ Token.exclusive,
                          Immediate <$ Token.immediate,
                          Deferred <$ perhaps_ Token.deferred
                        ]
                      <* optional Token.transaction
                  ),
            Statement'Commit <$ (choice [Token.commit, Token.end] *> optional Token.transaction),
            Statement'CreateIndex <$> createIndexStatement,
            Statement'CreateTable <$> createTableStatement,
            -- Statement'CreateTrigger <$> pure TODO,
            -- Statement'CreateView <$> pure TODO,
            -- Statement'CreateVirtualTable <$> pure TODO,
            Statement'Delete <$> deleteStatementRule,
            -- Statement'Detach <$> pure TODO,
            -- Statement'DropIndex <$> pure TODO,
            -- Statement'DropTable <$> pure TODO,
            -- Statement'DropTrigger <$> pure TODO,
            -- Statement'DropView <$> pure TODO,
            -- Statement'Insert <$> pure TODO,
            -- Statement'Pragma <$> pure TODO,
            -- Statement'Reindex <$> pure TODO,
            -- Statement'Release <$> pure TODO,
            Statement'Rollback
              <$> ( Token.rollback
                      *> optional Token.transaction
                      *> optional (Token.to *> optional Token.savepoint *> Token.identifier)
                  ),
            -- Statement'Savepoint <$> pure TODO,
            Statement'Select <$> selectStatementRule
            -- Statement'Update <$> pure TODO,
            -- Statement'Vacuum <$> pure TODO
          ]
          <* perhaps_ Token.semicolon

  pure
    Syntax
      { synExpression = expressionRule,
        synStatement = statementRule
      }
  where
    makeWindowRule :: Rule r Expression -> Rule r OrderingTerm -> Rule r Window
    makeWindowRule expressionRule orderingTermRule =
      parens do
        Window
          <$> optional Token.identifier
          <*> optional (Token.partition *> Token.by *> commaSep1 expressionRule)
          <*> optional (Token.order *> Token.by *> commaSep1 orderingTermRule)
          <*> choice [frameRule, pure defaultFrame]
      where
        defaultFrame :: Frame
        defaultFrame =
          Frame Range (Preceding Nothing) CurrentRow ExcludeNoOthers

        frameRule :: Rule r Frame
        frameRule =
          ( \type_ (startingBoundary, endingBoundary) exclude ->
              Frame {type_, startingBoundary, endingBoundary, exclude}
          )
            <$> choice
              [ Groups <$ Token.groups,
                Range <$ Token.range,
                Rows <$ Token.rows
              ]
            <*> frameBoundariesRule
            <*> frameExcludeRule
          where
            frameBoundariesRule :: Rule r (FrameBoundary Identity Maybe, FrameBoundary Maybe Identity)
            frameBoundariesRule =
              choice
                [ (,)
                    <$> ( Token.between
                            *> choice
                              [ currentRowRule,
                                exprFollowingRule Identity,
                                unboundedPrecedingRule,
                                exprPrecedingRule Just
                              ]
                        )
                    <*> ( Token.and
                            *> choice
                              [ currentRowRule,
                                exprFollowingRule Just,
                                unboundedFollowingRule,
                                exprPrecedingRule Identity
                              ]
                        ),
                  (,CurrentRow) <$> currentRowRule,
                  (,CurrentRow) <$> unboundedPrecedingRule,
                  (,CurrentRow) <$> exprPrecedingRule Just
                ]
              where
                currentRowRule :: Rule r (FrameBoundary f g)
                currentRowRule =
                  CurrentRow <$ (Token.current *> Token.row)

                exprFollowingRule :: (Expression -> f Expression) -> Rule r (FrameBoundary f g)
                exprFollowingRule f =
                  (Following . f) <$> (expressionRule <* Token.following)

                exprPrecedingRule :: (Expression -> g Expression) -> Rule r (FrameBoundary f g)
                exprPrecedingRule f =
                  (Preceding . f) <$> (expressionRule <* Token.preceding)

                unboundedFollowingRule :: Rule r (FrameBoundary Maybe g)
                unboundedFollowingRule =
                  Following Nothing <$ (Token.unbounded *> Token.following)

                unboundedPrecedingRule :: Rule r (FrameBoundary f Maybe)
                unboundedPrecedingRule =
                  Preceding Nothing <$ (Token.unbounded *> Token.preceding)

            frameExcludeRule :: Rule r FrameExclude
            frameExcludeRule =
              choice
                [ ExcludeCurrentRow <$ (Token.exclude *> Token.current *> Token.row),
                  ExcludeGroup <$ (Token.exclude *> Token.group),
                  ExcludeNoOthers <$ (Token.exclude *> Token.no *> Token.others),
                  ExcludeTies <$ (Token.exclude *> Token.ties),
                  pure ExcludeNoOthers
                ]

--

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

analyzeStatement :: Rule r AnalyzeStatement
analyzeStatement =
  AnalyzeStatement <$> (Token.analyze *> optional (namespacedRule Token.identifier Token.identifier))

makeAttachStatement :: Rule r Expression -> Rule r AttachStatement
makeAttachStatement expression =
  AttachStatement <$> (Token.attach *> optional Token.database *> expression) <*> (Token.as *> Token.identifier)

makeColumnDefinitionRule :: forall r. Rule r Expression -> Rule r ColumnDefinition
makeColumnDefinitionRule expressionRule =
  ColumnDefinition
    <$> Token.identifier
    <*> optional Token.identifier
    <*> many namedColumnConstraintRule
  where
    namedColumnConstraintRule :: Rule r (Named ColumnConstraint)
    namedColumnConstraintRule =
      Named
        <$> optional (Token.constraint *> Token.identifier)
        <*> columnConstraintRule
      where
        columnConstraintRule :: Rule r ColumnConstraint
        columnConstraintRule =
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
              ColumnConstraint'NotNull <$> (Token.not *> Token.null *> onConflictRule),
              ColumnConstraint'PrimaryKey
                <$> (Token.primary *> Token.key *> orderingRule)
                <*> onConflictRule
                <*> perhaps Token.autoincrement,
              ColumnConstraint'Unique <$> (Token.unique *> onConflictRule)
            ]
          where
            onConflictRule :: Rule r ConflictResolution
            onConflictRule =
              makeConflictResolutionRule (Token.on *> Token.conflict)

makeCreateIndexStatement :: Rule r Expression -> Rule r CreateIndexStatement
makeCreateIndexStatement expression =
  CreateIndexStatement
    <$> (Token.create *> perhaps Token.unique)
    <*> (Token.index *> perhaps (Token.if_ *> Token.not *> Token.exists))
    <*> namespacedRule Token.identifier Token.identifier
    <*> (Token.on *> Token.identifier)
    <*> parens (commaSep1 indexedColumnRule)
    <*> optional (Token.where_ *> expression)

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
        Right <$> tableDefinitionRule
      ]
  where
    tableDefinitionRule :: Rule r TableDefinition
    tableDefinitionRule =
      TableDefinition
        <$> (Token.leftParenthesis *> commaSep1 columnDefinition)
        <*> many (Token.comma *> namedTableConstraintRule)
        <* Token.rightParenthesis
        <*> perhaps (Token.without *> Token.rowid)
      where
        namedTableConstraintRule :: Rule r (Named TableConstraint)
        namedTableConstraintRule =
          Named
            <$> optional (Token.constraint *> Token.identifier)
            <*> tableConstraintRule
          where
            tableConstraintRule :: Rule r TableConstraint
            tableConstraintRule =
              choice
                [ TableConstraint'Check <$> (Token.check *> parens expression),
                  TableConstraint'ForeignKey
                    <$> (Token.foreign_ *> Token.key *> parens (commaSep1 Token.identifier))
                    <*> foreignKeyClauseRule,
                  TableConstraint'PrimaryKey
                    <$> (Token.primary *> Token.key *> parens (commaSep1 indexedColumnRule))
                    <*> onConflictRule,
                  TableConstraint'Unique
                    <$> (Token.unique *> parens (commaSep1 indexedColumnRule))
                    <*> onConflictRule
                ]
              where
                onConflictRule :: Rule r ConflictResolution
                onConflictRule =
                  makeConflictResolutionRule (Token.on *> Token.conflict)

makeExpressionRule :: Rule r SelectStatement -> Rule r Window -> Earley.Grammar r (Rule r Expression)
makeExpressionRule selectStatement windowRule = mdo
  filterClauseRule <- Earley.rule (Token.filter *> parens (Token.where_ *> expression))

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
                    <*> optional filterClauseRule
                ),
          Expression'Case <$> caseExpressionRule expression,
          Expression'Cast
            <$> ( CastExpression
                    <$> (Token.cast *> Token.leftParenthesis *> expression)
                    <*> (Token.as *> Token.identifier)
                ),
          Expression'Column
            <$> namespacedRule
              (Identity <$> namespacedRule Token.identifier Token.identifier)
              (Identity <$> Token.identifier),
          Expression'Exists <$> (Token.exists *> parens selectStatement),
          Expression'FunctionCall
            <$> ( FunctionCallExpression
                    <$> functionCallRule
                      ( choice
                          [ FunctionArguments'Arguments <$> commaSep0 expression,
                            FunctionArguments'Wildcard <$ Token.asterisk
                          ]
                      )
                    <*> optional filterClauseRule
                    <*> optional
                      ( Token.over
                          *> choice
                            [ Over'Window <$> windowRule,
                              Over'WindowName <$> Token.identifier
                            ]
                      )
                ),
          Expression'LiteralValue <$> literalValueRule,
          Expression'Parameter <$> parameterRule,
          Expression'Raise <$> raiseRule,
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

    caseExpressionRule :: Rule r Expression -> Rule r CaseExpression
    caseExpressionRule expressionRule =
      CaseExpression
        <$> (Token.case_ *> optional expressionRule)
        <*> some ((,) <$> (Token.when *> expressionRule) <*> (Token.then_ *> expressionRule))
        <*> choice
          [ Token.else_ *> expressionRule,
            pure (Expression'LiteralValue Null)
          ]
        <* Token.end

    parameterRule :: Rule r Parameter
    parameterRule =
      choice
        [ Parameter'Named <$> Token.namedParameter,
          Parameter'Ordinal <$> Token.parameter
        ]

makeConflictResolutionRule :: Rule r a -> Rule r ConflictResolution
makeConflictResolutionRule prefixRule =
  choice
    [ prefixRule
        *> choice
          [ Abort <$ Token.abort,
            Fail <$ Token.fail,
            Ignore <$ Token.ignore,
            Replace <$ Token.replace,
            Rollback <$ Token.rollback
          ],
      pure Abort
    ]

generatedType :: Rule r GeneratedType
generatedType =
  choice
    [ Stored <$ Token.stored,
      Virtual <$ Token.virtual
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

raiseRule :: Rule r Raise
raiseRule =
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

signRule :: Rule r Sign
signRule =
  choice
    [ Sign'HyphenMinus <$ Token.hyphenMinus,
      Sign'PlusSign <$ Token.plusSign
    ]

signedNumber :: Rule r SignedNumber
signedNumber =
  SignedNumber
    <$> optional signRule
    <*> Token.number
