module Sqlite.Syntax.Internal.Parser.Rule.ColumnDefinition
  ( makeColumnDefinitionRule,
  )
where

import Control.Applicative (many, optional)
import Control.Applicative.Combinators (choice)
import Sqlite.Syntax
import Sqlite.Syntax.Internal.Parser.Rule.ConflictResolution (makeConflictResolutionRule)
import Sqlite.Syntax.Internal.Parser.Rule.ForeignKeyClause (foreignKeyClauseRule)
import Sqlite.Syntax.Internal.Parser.Rule.LiteralValue (literalValueRule)
import Sqlite.Syntax.Internal.Parser.Rule.Ordering (orderingRule)
import Sqlite.Syntax.Internal.Parser.Utils
import qualified Sqlite.Syntax.Parser.Token as Token
import Prelude

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
                    Default'SignedNumber <$> signedNumberRule
                  ],
              ColumnConstraint'ForeignKey <$> foreignKeyClauseRule,
              ColumnConstraint'Generated
                <$> (optional (Token.generated *> Token.always) *> Token.as *> parens expressionRule)
                <*> generatedTypeRule,
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

generatedTypeRule :: Rule r GeneratedType
generatedTypeRule =
  choice
    [ Stored <$ Token.stored,
      Virtual <$ Token.virtual,
      pure Virtual
    ]

signRule :: Rule r Sign
signRule =
  choice
    [ Sign'HyphenMinus <$ Token.hyphenMinus,
      Sign'PlusSign <$ Token.plusSign
    ]

signedNumberRule :: Rule r SignedNumber
signedNumberRule =
  maybe UnsignedNumber SignedNumber
    <$> optional signRule
    <*> Token.number
