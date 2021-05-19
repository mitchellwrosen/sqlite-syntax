module Lexer
  ( Token (..),
    lex,
  )
where

import Control.Applicative (many, (<|>))
import Control.Monad.Combinators (choice)
import Data.Char
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import TextParser (TextParser)
import qualified TextParser
import Prelude hiding (Bool (..), exponent, lex)

{- ORMOLU_DISABLE -}
data Token
  = ABORT -- ^ @ABORT@
  | ACTION -- ^ @ACTION@
  | ADD -- ^ @ADD@
  | AFTER -- ^ @AFTER@
  | ALL -- ^ @ALL@
  | ALTER -- ^ @ALTER@
  | ALWAYS -- ^ @ALWAYS@
  | ANALYZE -- ^ @ANALYZE@
  | AND -- ^ @AND@
  | AS -- ^ @AS@
  | ASC -- ^ @ASC@
  | ATTACH -- ^ @ATTACH@
  | AUTOINCREMENT -- ^ @AUTOINCREMENT@
  | Ampersand -- ^ @&@
  | Asterisk -- ^ @*@
  | BEFORE -- ^ @BEFORE@
  | BEGIN -- ^ @BEGIN@
  | BETWEEN -- ^ @BETWEEN@
  | BY -- ^ @BY@
  | Blob Text -- ^ @x'ff'@
  | CASCADE -- ^ @CASCADE@
  | CASE -- ^ @CASE@
  | CAST -- ^ @CAST@
  | CHECK -- ^ @CHECK@
  | COLLATE -- ^ @COLLATE@
  | COLUMN -- ^ @COLUMN@
  | COMMIT -- ^ @COMMIT@
  | CONFLICT -- ^ @CONFLICT@
  | CONSTRAINT -- ^ @CONSTRAINT@
  | CREATE -- ^ @CREATE@
  | CROSS -- ^ @CROSS@
  | CURRENT -- ^ @CURRENT@
  | CURRENT_DATE -- ^ @CURRENT_DATE@
  | CURRENT_TIME -- ^ @CURRENT_TIME@
  | CURRENT_TIMESTAMP -- ^ @CURRENT_TIMESTAMP@
  | Comma -- ^ @,@
  | DATABASE -- ^ @DATABASE@
  | DEFAULT -- ^ @DEFAULT@
  | DEFERRABLE -- ^ @DEFERRABLE@
  | DEFERRED -- ^ @DEFERRED@
  | DELETE -- ^ @DELETE@
  | DESC -- ^ @DESC@
  | DETACH -- ^ @DETACH@
  | DISTINCT -- ^ @DISTINCT@
  | DO -- ^ @DO@
  | DROP -- ^ @DROP@
  | EACH -- ^ @EACH@
  | ELSE -- ^ @ELSE@
  | END -- ^ @END@
  | ESCAPE -- ^ @ESCAPE@
  | EXCEPT -- ^ @EXCEPT@
  | EXCLUDE -- ^ @EXCLUDE@
  | EXCLUSIVE -- ^ @EXCLUSIVE@
  | EXISTS -- ^ @EXISTS@
  | EXPLAIN -- ^ @EXPLAIN@
  | EqualsSign -- ^ @=@
  | EqualsSignEqualsSign -- ^ @==@
  | ExclamationMarkEqualsSign -- ^ @!=@
  | FAIL -- ^ @FAIL@
  | FALSE -- ^ @FALSE@
  | FILTER -- ^ @FILTER@
  | FIRST -- ^ @FIRST@
  | FOLLOWING -- ^ @FOLLOWING@
  | FOR -- ^ @FOR@
  | FOREIGN -- ^ @FOREIGN@
  | FROM -- ^ @FROM@
  | FULL -- ^ @FULL@
  | FullStop -- ^ @.@
  | GENERATED -- ^ @GENERATED@
  | GLOB -- ^ @GLOB@
  | GROUP -- ^ @GROUP@
  | GROUPS -- ^ @GROUPS@
  | GreaterThanSign -- ^ @>@
  | GreaterThanSignEqualsSign -- ^ @>=@
  | GreaterThanSignGreaterThanSign -- ^ @>>@
  | HAVING -- ^ @HAVING@
  | HyphenMinus -- ^ @-@
  | IF -- ^ @IF@
  | IGNORE -- ^ @IGNORE@
  | IMMEDIATE -- ^ @IMMEDIATE@
  | IN -- ^ @IN@
  | INDEX -- ^ @INDEX@
  | INDEXED -- ^ @INDEXED@
  | INITIALLY -- ^ @INITIALLY@
  | INNER -- ^ @INNER@
  | INSERT -- ^ @INSERT@
  | INSTEAD -- ^ @INSTEAD@
  | INTERSECT -- ^ @INTERSECT@
  | INTO -- ^ @INTO@
  | IS -- ^ @IS@
  | ISNULL -- ^ @ISNULL@
  | Identifier Text
  | JOIN -- ^ @JOIN@
  | KEY -- ^ @KEY@
  | LAST -- ^ @LAST@
  | LEFT -- ^ @LEFT@
  | LIKE -- ^ @LIKE@
  | LIMIT -- ^ @LIMIT@
  | LeftParen -- ^ @(@
  | LessThanSign -- ^ @<@
  | LessThanSignEqualsSign -- ^ @<=@
  | LessThanSignGreaterThanSign -- ^ @<>@
  | LessThanSignLessThanSign -- ^ @<<@
  | MATCH -- ^ @MATCH@
  | MATERIALIZED -- ^ @MATERIALIZED@
  | NATURAL -- ^ @NATURAL@
  | NO -- ^ @NO@
  | NOT -- ^ @NOT@
  | NOTHING -- ^ @NOTHING@
  | NOTNULL -- ^ @NOTNULL@
  | NULL -- ^ @NULL@
  | NULLS -- ^ @NULLS@
  | Number Text -- ^ Numeric literal
  | OF -- ^ @OF@
  | OFFSET -- ^ @OFFSET@
  | ON -- ^ @ON@
  | OR -- ^ @OR@
  | ORDER -- ^ @ORDER@
  | OTHERS -- ^ @OTHERS@
  | OUTER -- ^ @OUTER@
  | OVER -- ^ @OVER@
  | PARTITION -- ^ @PARTITION@
  | PLAN -- ^ @PLAN@
  | PRAGMA -- ^ @PRAGMA@
  | PRECEDING -- ^ @PRECEDING@
  | PRIMARY -- ^ @PRIMARY@
  | PercentSign -- ^ @%@
  | PlusSign -- ^ @+@
  | QUERY -- ^ @QUERY@
  | RAISE -- ^ @RAISE@
  | RANGE -- ^ @RANGE@
  | RECURSIVE -- ^ @RECURSIVE@
  | REFERENCES -- ^ @REFERENCES@
  | REGEXP -- ^ @REGEXP@
  | REINDEX -- ^ @REINDEX@
  | RELEASE -- ^ @RELEASE@
  | RENAME -- ^ @RENAME@
  | REPLACE -- ^ @REPLACE@
  | RESTRICT -- ^ @RESTRICT@
  | RETURNING -- ^ @RETURNING@
  | RIGHT -- ^ @RIGHT@
  | ROLLBACK -- ^ @ROLLBACK@
  | ROW -- ^ @ROW@
  | ROWID -- ^ @ROWID@
  | ROWS -- ^ @ROWS@
  | RightParen -- ^ @)@
  | SAVEPOINT -- ^ @SAVEPOINT@
  | SELECT -- ^ @SELECT@
  | SET -- ^ @SET@
  | Semicolon -- ^ @;@
  | Solidus -- ^ @/@
  | String Text
  | TABLE -- ^ @TABLE@
  | TEMP -- ^ @TEMP@
  | TEMPORARY -- ^ @TEMPORARY@
  | THEN -- ^ @THEN@
  | TIES -- ^ @TIES@
  | TO -- ^ @TO@
  | TRANSACTION -- ^ @TRANSACTION@
  | TRIGGER -- ^ @TRIGGER@
  | TRUE -- ^ @TRUE@
  | Tilde -- ^ @~@
  | UNBOUNDED -- ^ @UNBOUNDED@
  | UNION -- ^ @UNION@
  | UNIQUE -- ^ @UNIQUE@
  | UPDATE -- ^ @UPDATE@
  | USING -- ^ @USING@
  | VACUUM -- ^ @VACUUM@
  | VALUES -- ^ @VALUES@
  | VIEW -- ^ @VIEW@
  | VIRTUAL -- ^ @VIRTUAL@
  | VerticalLine -- ^ @|@
  | VerticalLineVerticalLine -- ^ @||@
  | WHEN -- ^ @WHEN@
  | WHERE -- ^ @WHERE@
  | WINDOW -- ^ @WINDOW@
  | WITH -- ^ @WITH@
  | WITHOUT -- ^ @WITHOUT@
  deriving stock (Eq, Generic, Show)
{- ORMOLU_ENABLE -}

keyword :: Text -> TextParser Text
keyword s =
  TextParser.string' s <* space

space :: TextParser ()
space =
  TextParser.space "--" "/*" "*/"

symbol :: Text -> TextParser Text
symbol s =
  TextParser.string s <* space

lex :: Text -> Either Text [Token]
lex =
  TextParser.run do
    space
    tokens <- many token
    TextParser.eof
    pure tokens

token :: TextParser Token
token =
  choice
    [ Ampersand <$ symbol "&",
      Asterisk <$ symbol "*",
      Comma <$ symbol ",",
      EqualsSignEqualsSign <$ symbol "==",
      EqualsSign <$ symbol "=",
      ExclamationMarkEqualsSign <$ symbol "!=",
      Number <$> number,
      FullStop <$ symbol ".",
      GreaterThanSignEqualsSign <$ symbol ">=",
      GreaterThanSignGreaterThanSign <$ symbol ">>",
      GreaterThanSign <$ symbol ">",
      HyphenMinus <$ symbol "-",
      LeftParen <$ symbol "(",
      LessThanSignEqualsSign <$ symbol "<=",
      LessThanSignGreaterThanSign <$ symbol "<>",
      LessThanSignLessThanSign <$ symbol "<<",
      LessThanSign <$ symbol "<",
      PercentSign <$ symbol "%",
      PlusSign <$ symbol "+",
      RightParen <$ symbol ")",
      Semicolon <$ symbol ";",
      Solidus <$ symbol "/",
      Tilde <$ symbol "~",
      VerticalLineVerticalLine <$ symbol "||",
      VerticalLine <$ symbol "|",
      Blob <$> blob,
      Identifier <$> identifier,
      ABORT <$ keyword "abort",
      ACTION <$ keyword "action",
      ADD <$ keyword "add",
      AFTER <$ keyword "after",
      ALL <$ keyword "all",
      ALTER <$ keyword "alter",
      ALWAYS <$ keyword "always",
      ANALYZE <$ keyword "analyze",
      AND <$ keyword "and",
      ASC <$ keyword "asc",
      AS <$ keyword "as",
      ATTACH <$ keyword "attach",
      AUTOINCREMENT <$ keyword "autoincrement",
      BEFORE <$ keyword "before",
      BEGIN <$ keyword "begin",
      BETWEEN <$ keyword "between",
      BY <$ keyword "by",
      CASCADE <$ keyword "cascade",
      CASE <$ keyword "case",
      CAST <$ keyword "cast",
      CHECK <$ keyword "check",
      COLLATE <$ keyword "collate",
      COLUMN <$ keyword "column",
      COMMIT <$ keyword "commit",
      CONFLICT <$ keyword "conflict",
      CONSTRAINT <$ keyword "constraint",
      CREATE <$ keyword "create",
      CROSS <$ keyword "cross",
      CURRENT_DATE <$ keyword "current_date",
      CURRENT_TIMESTAMP <$ keyword "current_timestamp",
      CURRENT_TIME <$ keyword "current_time",
      CURRENT <$ keyword "current",
      DATABASE <$ keyword "database",
      DEFAULT <$ keyword "default",
      DEFERRABLE <$ keyword "deferrable",
      DEFERRED <$ keyword "deferred",
      DELETE <$ keyword "delete",
      DESC <$ keyword "desc",
      DETACH <$ keyword "detach",
      DISTINCT <$ keyword "distinct",
      DO <$ keyword "do",
      DROP <$ keyword "drop",
      EACH <$ keyword "each",
      ELSE <$ keyword "else",
      END <$ keyword "end",
      ESCAPE <$ keyword "escape",
      EXCEPT <$ keyword "except",
      EXCLUDE <$ keyword "exclude",
      EXCLUSIVE <$ keyword "exclusive",
      EXISTS <$ keyword "exists",
      EXPLAIN <$ keyword "explain",
      FAIL <$ keyword "fail",
      FALSE <$ keyword "false",
      FILTER <$ keyword "filter",
      FIRST <$ keyword "first",
      FOLLOWING <$ keyword "following",
      FOREIGN <$ keyword "foreign",
      FOR <$ keyword "for",
      FROM <$ keyword "from",
      FULL <$ keyword "full",
      GENERATED <$ keyword "generated",
      GLOB <$ keyword "glob",
      GROUPS <$ keyword "groups",
      GROUP <$ keyword "group",
      HAVING <$ keyword "having",
      IF <$ keyword "if",
      IGNORE <$ keyword "ignore",
      IMMEDIATE <$ keyword "immediate",
      INDEXED <$ keyword "indexed",
      INDEX <$ keyword "index",
      INITIALLY <$ keyword "initially",
      INNER <$ keyword "inner",
      INSERT <$ keyword "insert",
      INSTEAD <$ keyword "instead",
      INTERSECT <$ keyword "intersect",
      INTO <$ keyword "into",
      IN <$ keyword "in",
      ISNULL <$ keyword "isnull",
      IS <$ keyword "is",
      JOIN <$ keyword "join",
      KEY <$ keyword "key",
      LAST <$ keyword "last",
      LEFT <$ keyword "left",
      LIKE <$ keyword "like",
      LIMIT <$ keyword "limit",
      MATCH <$ keyword "match",
      MATERIALIZED <$ keyword "materialized",
      NATURAL <$ keyword "natural",
      NOTHING <$ keyword "nothing",
      NOTNULL <$ keyword "notnull",
      NOT <$ keyword "not",
      NO <$ keyword "no",
      NULLS <$ keyword "nulls",
      NULL <$ keyword "null",
      OFFSET <$ keyword "offset",
      OF <$ keyword "of",
      ON <$ keyword "on",
      ORDER <$ keyword "order",
      OR <$ keyword "or",
      OTHERS <$ keyword "others",
      OUTER <$ keyword "outer",
      OVER <$ keyword "over",
      PARTITION <$ keyword "partition",
      PLAN <$ keyword "plan",
      PRAGMA <$ keyword "pragma",
      PRECEDING <$ keyword "preceding",
      PRIMARY <$ keyword "primary",
      QUERY <$ keyword "query",
      RAISE <$ keyword "raise",
      RANGE <$ keyword "range",
      RECURSIVE <$ keyword "recursive",
      REFERENCES <$ keyword "references",
      REGEXP <$ keyword "regexp",
      REINDEX <$ keyword "reindex",
      RELEASE <$ keyword "release",
      RENAME <$ keyword "rename",
      REPLACE <$ keyword "replace",
      RESTRICT <$ keyword "restrict",
      RETURNING <$ keyword "returning",
      RIGHT <$ keyword "right",
      ROLLBACK <$ keyword "rollback",
      ROWID <$ keyword "rowid",
      ROWS <$ keyword "rows",
      ROW <$ keyword "row",
      SAVEPOINT <$ keyword "savepoint",
      SELECT <$ keyword "select",
      SET <$ keyword "set",
      TABLE <$ keyword "table",
      TEMPORARY <$ keyword "temporary",
      TEMP <$ keyword "temp",
      THEN <$ keyword "then",
      TIES <$ keyword "ties",
      TO <$ keyword "to",
      TRANSACTION <$ keyword "transaction",
      TRIGGER <$ keyword "trigger",
      TRUE <$ keyword "true",
      UNBOUNDED <$ keyword "unbounded",
      UNION <$ keyword "union",
      UNIQUE <$ keyword "unique",
      UPDATE <$ keyword "update",
      USING <$ keyword "using",
      VACUUM <$ keyword "vacuum",
      VALUES <$ keyword "values",
      VIEW <$ keyword "view",
      VIRTUAL <$ keyword "virtual",
      WHEN <$ keyword "when",
      WHERE <$ keyword "where",
      WINDOW <$ keyword "window",
      WITHOUT <$ keyword "without",
      WITH <$ keyword "with",
      String <$> string
      -- TODO parameters
    ]
  where
    blob :: TextParser Text
    blob = do
      _ <- TextParser.string' "X\'" <|> TextParser.string' "x\'"
      TextParser.commit
      s <- TextParser.takeWhile isHexDigit
      _ <- TextParser.char '\''
      if even (Text.length s)
        then s <$ space
        else fail "invalid hex data"

    -- It appears any string is a valid identifier, even the empty string, except strings that begin with "sqlite_".
    -- There are three different ways of quoting identifiers ("this", [this], and `this`), and within a quoted
    -- identifier, I don't believe there is any escape syntax. (It seems impossible, therefore, to define an identifer
    -- that happens to have a ", ], and ` character...). Still looking for official documentation on all of this. This
    -- is all I've found:
    --
    --   https://stackoverflow.com/questions/3694276/what-are-valid-table-names-in-sqlite
    identifier :: TextParser Text
    identifier = do
      s <- unquoted <|> quoted '"' '"' <|> quoted '[' ']' <|> quoted '`' '`'
      if "sqlite_" `Text.isPrefixOf` s
        then fail "reserved identifier"
        else do
          space
          pure s
      where
        quoted :: Char -> Char -> TextParser Text
        quoted c0 c1 = do
          _ <- TextParser.char c0
          TextParser.commit
          s <- TextParser.takeWhile \c -> '\x20' <= c && c <= '\x10FFFF' && c /= c1
          _ <- TextParser.char c1
          pure s

        -- This is just a guess for now. Still looking for official documentation.
        unquoted :: TextParser Text
        unquoted = do
          x <- TextParser.satisfy isLower
          TextParser.commit
          xs <- TextParser.takeWhile (\c -> isAlphaNum c || c == '_')
          pure (Text.cons x xs)

    number :: TextParser Text
    number =
      choice
        [ do
            s0 <- TextParser.string' "0x"
            TextParser.commit
            s1 <- TextParser.takeWhile1 isHexDigit
            pure (s0 <> s1),
          do
            s0 <- TextParser.takeWhile isDigit
            s1 <-
              if Text.null s0
                then do
                  TextParser.char_ '.'
                  s1 <- digits
                  TextParser.commit
                  pure (Text.cons '.' s1)
                else do
                  TextParser.commit
                  ( do
                      TextParser.char_ '.'
                      TextParser.commit
                      s1 <- digits
                      pure (Text.cons '.' s1)
                    )
                    <|> pure ""
            s2 <- exponent <|> pure ""
            pure (s0 <> s1 <> s2)
        ]
        <* space
      where
        digits :: TextParser Text
        digits =
          TextParser.takeWhile1 isDigit

        exponent :: TextParser Text
        exponent = do
          s0 <- TextParser.satisfy \c -> c == 'e' || c == 'E'
          TextParser.commit
          s1 <- sign <|> pure ""
          s2 <- digits
          pure (Text.cons s0 (s1 <> s2))
          where
            sign :: TextParser Text
            sign =
              Text.singleton <$> TextParser.satisfy \c -> c == '+' || c == '-'

    -- A string constant is formed by enclosing the string in single quotes ('). A single quote within the string can be
    -- encoded by putting two single quotes in a row - as in Pascal. C-style escapes using the backslash character are
    -- not supported because they are not standard SQL.
    string :: TextParser Text
    string = do
      _ <- TextParser.char '\''
      TextParser.commit
      chunks <- many (chunk <|> singleQuote)
      _ <- TextParser.char '\''
      space
      pure (Text.concat chunks)
      where
        chunk :: TextParser Text
        chunk =
          TextParser.takeWhile1 \c -> ' ' <= c && c <= '\x10FFFF' && c /= '\''

        singleQuote :: TextParser Text
        singleQuote =
          "'" <$ TextParser.string "''"
