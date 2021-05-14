module Lexer
  ( Token (..),
    lex,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (many)
import Data.ByteString (ByteString)
import Data.Char
import Data.Foldable (asum)
import Data.Functor
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Megaparsec.Lexer
import Prelude hiding (Bool (..), lex)

type Lexer =
  Megaparsec.Parsec Void Text

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
  | Blob ByteString
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
  | Float Double
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
  | Integer Int64
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

keyword :: Text -> Lexer Text
keyword =
  Megaparsec.Lexer.symbol' space

lexeme :: Lexer a -> Lexer a
lexeme =
  Megaparsec.Lexer.lexeme space

space :: Lexer ()
space =
  Megaparsec.Lexer.space
    Megaparsec.space1
    (Megaparsec.Lexer.skipLineComment "--")
    (Megaparsec.Lexer.skipBlockComment "/*" "*/")

symbol :: Text -> Lexer Text
symbol =
  Megaparsec.Lexer.symbol space

lex :: Text -> Either Text [Token]
lex input =
  case Megaparsec.parse lexer "" input of
    Left err -> Left (Text.pack (Megaparsec.errorBundlePretty err))
    Right tokens -> Right tokens
  where
    lexer :: Lexer [Token]
    lexer = do
      space
      tokens <- many token
      Megaparsec.eof
      pure tokens

token :: Lexer Token
token =
  asum
    [ Ampersand <$ symbol "&",
      Asterisk <$ symbol "*",
      Comma <$ symbol ",",
      EqualsSignEqualsSign <$ symbol "==",
      EqualsSign <$ symbol "=",
      ExclamationMarkEqualsSign <$ symbol "!=",
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
      -- Blob <$> undefined,
      -- Integer <$> undefined,
      -- Float <$> undefined,
      String <$> string
      -- TODO parameters
    ]
  where
    -- It appears any string is a valid identifier, even the empty string, except strings that begin with "sqlite_".
    -- There are three different ways of quoting identifiers ("this", [this], and `this`), and within a quoted
    -- identifier, I don't believe there is any escape syntax. (It seems impossible, therefore, to define an identifer
    -- that happens to have a ", ], and ` character...). Still looking for official documentation on all of this. This is
    -- all I've found:
    --
    --   https://stackoverflow.com/questions/3694276/what-are-valid-table-names-in-sqlite
    identifier :: Lexer Text
    identifier = do
      lexeme do
        s <- unquoted <|> quoted '"' '"' <|> quoted '[' ']' <|> quoted '`' '`'
        if "sqlite_" `Text.isPrefixOf` s
          then fail "reserved identifier"
          else pure s
      where
        quoted :: Char -> Char -> Lexer Text
        quoted c0 c1 = do
          _ <- Megaparsec.char c0
          s <- Megaparsec.takeWhileP Nothing \c -> '\x20' <= c && c <= '\x10FFFF' && c /= c1
          _ <- Megaparsec.char c1
          pure s

        -- This is just a guess for now. Still looking for official documentation.
        unquoted :: Lexer Text
        unquoted = do
          x <- Megaparsec.lowerChar
          xs <- Megaparsec.takeWhileP Nothing (\c -> isAlphaNum c || c == '_')
          pure (Text.cons x xs)

    -- A string constant is formed by enclosing the string in single quotes ('). A single quote within the string can be
    -- encoded by putting two single quotes in a row - as in Pascal. C-style escapes using the backslash character are
    -- not supported because they are not standard SQL.
    string :: Lexer Text
    string =
      lexeme do
        _ <- Megaparsec.char '\''
        chunks <- many (chunk <|> singleQuote)
        _ <- Megaparsec.char '\''
        pure (Text.concat chunks)
      where
        chunk :: Lexer Text
        chunk =
          Megaparsec.takeWhile1P Nothing \c -> '\x20' <= c && c <= '\x10FFFF' && c /= '\''

        singleQuote :: Lexer Text
        singleQuote =
          "'" <$ Megaparsec.string "''"
