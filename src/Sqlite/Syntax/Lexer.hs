-- Here's something: https://www.sqlite.org/draft/tokenreq.html
module Sqlite.Syntax.Lexer
  ( lex,
  )
where

import Control.Applicative (many, (<|>))
import Control.Monad.Combinators (choice, optional)
import Data.Char
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import Numeric.Natural (Natural)
import Sqlite.Syntax.Token (Token (..))
import TextParser (TextParser)
import qualified TextParser
import Prelude hiding (exponent, lex)

lex :: Text -> Either Text [Token]
lex =
  TextParser.run do
    space
    tokens <- many token
    TextParser.eof
    pure tokens

--

space :: TextParser ()
space =
  TextParser.space "--" "/*" "*/"

symbol :: Text -> TextParser Text
symbol s =
  TextParser.string s <* space

quoted :: Char -> TextParser Text
quoted c0 = do
  _ <- TextParser.char c0
  TextParser.commit
  chunks <- many (chunk <|> quote)
  _ <- TextParser.char c0
  pure (Text.concat chunks)
  where
    chunk :: TextParser Text
    chunk =
      TextParser.takeWhile1 (/= c0)

    quote :: TextParser Text
    quote =
      Text.singleton c0 <$ TextParser.string (Text.pack [c0, c0])

--

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
      LeftParenthesis <$ symbol "(",
      LessThanSignEqualsSign <$ symbol "<=",
      LessThanSignGreaterThanSign <$ symbol "<>",
      LessThanSignLessThanSign <$ symbol "<<",
      LessThanSign <$ symbol "<",
      PercentSign <$ symbol "%",
      PlusSign <$ symbol "+",
      RightParenthesis <$ symbol ")",
      Semicolon <$ symbol ";",
      Solidus <$ symbol "/",
      Tilde <$ symbol "~",
      VerticalLineVerticalLine <$ symbol "||",
      VerticalLine <$ symbol "|",
      Blob <$> blob,
      String <$> string,
      unquotedIdentifier,
      quotedIdentifier,
      Parameter <$> parameter,
      NamedParameter <$> namedParameter
    ]

blob :: TextParser Text
blob = do
  _ <- TextParser.string' "X\'" <|> TextParser.string' "x\'"
  TextParser.commit
  s <- TextParser.takeWhile isHexDigit
  _ <- TextParser.char '\''
  space
  if even (Text.length s)
    then pure s
    else fail "invalid hex data"

namedParameter :: TextParser Text
namedParameter = do
  s0 <- TextParser.satisfy \c -> c == ':' || c == '@' || c == '$'
  TextParser.commit
  s1 <- parameterName
  space
  pure (if s0 == '@' then Text.cons s0 s1 else s1)
  where
    -- A "parameter name" is defined to be a sequence of one or more characters that consists of ALPHANUMERIC characters
    -- and/or dollar-signs (u0025) intermixed with pairs of colons (u003a) and optionally followed by any sequence of
    -- non-zero, non-WHITESPACE characters enclosed in parentheses (u0028 and u0029).
    parameterName :: TextParser Text
    parameterName = do
      chunks <- many (TextParser.takeWhile1 isAlphaNum <|> TextParser.string "$" <|> TextParser.string "::")
      maybeSuffix <-
        optional do
          c0 <- TextParser.string "("
          TextParser.commit
          c1 <- TextParser.takeWhile (\c -> not (isSpace c) && c /= ')')
          c2 <- TextParser.string ")"
          pure (Text.concat [c0, c1, c2])
      pure (Text.concat (maybe id (\suffix -> (++ [suffix])) maybeSuffix chunks))

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

parameter :: TextParser (Maybe Natural)
parameter = do
  _ <- TextParser.char '?'
  TextParser.commit
  digits <- TextParser.takeWhile isDigit
  space
  pure do
    case Text.Read.decimal digits of
      Left _ -> Nothing
      Right (n0, _) -> Just n0

quotedIdentifier :: TextParser Token
quotedIdentifier = do
  s <- quoted '"' <|> bracketQuoted <|> quoted '`'
  space
  makeIdentifier s
  where
    bracketQuoted :: TextParser Text
    bracketQuoted = do
      _ <- TextParser.char '['
      TextParser.commit
      s <- TextParser.takeWhile (/= ']')
      _ <- TextParser.char ']'
      pure s

-- A string constant is formed by enclosing the string in single quotes ('). A single quote within the string can be
-- encoded by putting two single quotes in a row - as in Pascal. C-style escapes using the backslash character are
-- not supported because they are not standard SQL.
string :: TextParser Text
string =
  quoted '\'' <* space

-- This is kind of a guess. Still looking for official documentation.
unquotedIdentifier :: TextParser Token
unquotedIdentifier = do
  x <- TextParser.satisfy isAlpha
  TextParser.commit
  xs <- TextParser.takeWhile (\c -> isAlphaNum c || c == '$' || c == '_')
  space
  fromIdentifier (Text.cons x xs)
  where
    fromIdentifier :: Text -> TextParser Token
    fromIdentifier s
      | s' == "abort" = pure ABORT
      | s' == "action" = pure ACTION
      | s' == "add" = pure ADD
      | s' == "after" = pure AFTER
      | s' == "all" = pure ALL
      | s' == "alter" = pure ALTER
      | s' == "always" = pure ALWAYS
      | s' == "analyze" = pure ANALYZE
      | s' == "and" = pure AND
      | s' == "asc" = pure ASC
      | s' == "as" = pure AS
      | s' == "attach" = pure ATTACH
      | s' == "autoincrement" = pure AUTOINCREMENT
      | s' == "before" = pure BEFORE
      | s' == "begin" = pure BEGIN
      | s' == "between" = pure BETWEEN
      | s' == "by" = pure BY
      | s' == "cascade" = pure CASCADE
      | s' == "case" = pure CASE
      | s' == "cast" = pure CAST
      | s' == "check" = pure CHECK
      | s' == "collate" = pure COLLATE
      | s' == "column" = pure COLUMN
      | s' == "commit" = pure COMMIT
      | s' == "conflict" = pure CONFLICT
      | s' == "constraint" = pure CONSTRAINT
      | s' == "create" = pure CREATE
      | s' == "cross" = pure CROSS
      | s' == "current_date" = pure CURRENT_DATE
      | s' == "current_timestamp" = pure CURRENT_TIMESTAMP
      | s' == "current_time" = pure CURRENT_TIME
      | s' == "current" = pure CURRENT
      | s' == "database" = pure DATABASE
      | s' == "default" = pure DEFAULT
      | s' == "deferrable" = pure DEFERRABLE
      | s' == "deferred" = pure DEFERRED
      | s' == "delete" = pure DELETE
      | s' == "desc" = pure DESC
      | s' == "detach" = pure DETACH
      | s' == "distinct" = pure DISTINCT
      | s' == "do" = pure DO
      | s' == "drop" = pure DROP
      | s' == "each" = pure EACH
      | s' == "else" = pure ELSE
      | s' == "end" = pure END
      | s' == "escape" = pure ESCAPE
      | s' == "except" = pure EXCEPT
      | s' == "exclude" = pure EXCLUDE
      | s' == "exclusive" = pure EXCLUSIVE
      | s' == "exists" = pure EXISTS
      | s' == "explain" = pure EXPLAIN
      | s' == "fail" = pure FAIL
      | s' == "false" = pure FALSE
      | s' == "filter" = pure FILTER
      | s' == "first" = pure FIRST
      | s' == "following" = pure FOLLOWING
      | s' == "foreign" = pure FOREIGN
      | s' == "for" = pure FOR
      | s' == "from" = pure FROM
      | s' == "full" = pure FULL
      | s' == "generated" = pure GENERATED
      | s' == "glob" = pure GLOB
      | s' == "groups" = pure GROUPS
      | s' == "group" = pure GROUP
      | s' == "having" = pure HAVING
      | s' == "if" = pure IF
      | s' == "ignore" = pure IGNORE
      | s' == "immediate" = pure IMMEDIATE
      | s' == "indexed" = pure INDEXED
      | s' == "index" = pure INDEX
      | s' == "initially" = pure INITIALLY
      | s' == "inner" = pure INNER
      | s' == "insert" = pure INSERT
      | s' == "instead" = pure INSTEAD
      | s' == "intersect" = pure INTERSECT
      | s' == "into" = pure INTO
      | s' == "in" = pure IN
      | s' == "isnull" = pure ISNULL
      | s' == "is" = pure IS
      | s' == "join" = pure JOIN
      | s' == "key" = pure KEY
      | s' == "last" = pure LAST
      | s' == "left" = pure LEFT
      | s' == "like" = pure LIKE
      | s' == "limit" = pure LIMIT
      | s' == "match" = pure MATCH
      | s' == "materialized" = pure MATERIALIZED
      | s' == "natural" = pure NATURAL
      | s' == "nothing" = pure NOTHING
      | s' == "notnull" = pure NOTNULL
      | s' == "not" = pure NOT
      | s' == "no" = pure NO
      | s' == "nulls" = pure NULLS
      | s' == "null" = pure NULL
      | s' == "offset" = pure OFFSET
      | s' == "of" = pure OF
      | s' == "on" = pure ON
      | s' == "order" = pure ORDER
      | s' == "or" = pure OR
      | s' == "others" = pure OTHERS
      | s' == "outer" = pure OUTER
      | s' == "over" = pure OVER
      | s' == "partition" = pure PARTITION
      | s' == "plan" = pure PLAN
      | s' == "pragma" = pure PRAGMA
      | s' == "preceding" = pure PRECEDING
      | s' == "primary" = pure PRIMARY
      | s' == "query" = pure QUERY
      | s' == "raise" = pure RAISE
      | s' == "range" = pure RANGE
      | s' == "recursive" = pure RECURSIVE
      | s' == "references" = pure REFERENCES
      | s' == "regexp" = pure REGEXP
      | s' == "reindex" = pure REINDEX
      | s' == "release" = pure RELEASE
      | s' == "rename" = pure RENAME
      | s' == "replace" = pure REPLACE
      | s' == "restrict" = pure RESTRICT
      | s' == "returning" = pure RETURNING
      | s' == "right" = pure RIGHT
      | s' == "rollback" = pure ROLLBACK
      | s' == "rows" = pure ROWS
      | s' == "row" = pure ROW
      | s' == "savepoint" = pure SAVEPOINT
      | s' == "select" = pure SELECT
      | s' == "set" = pure SET
      | s' == "stored" = pure STORED
      | s' == "table" = pure TABLE
      | s' == "temporary" = pure TEMPORARY
      | s' == "temp" = pure TEMP
      | s' == "then" = pure THEN
      | s' == "ties" = pure TIES
      | s' == "to" = pure TO
      | s' == "transaction" = pure TRANSACTION
      | s' == "trigger" = pure TRIGGER
      | s' == "true" = pure TRUE
      | s' == "unbounded" = pure UNBOUNDED
      | s' == "union" = pure UNION
      | s' == "unique" = pure UNIQUE
      | s' == "update" = pure UPDATE
      | s' == "using" = pure USING
      | s' == "vacuum" = pure VACUUM
      | s' == "values" = pure VALUES
      | s' == "view" = pure VIEW
      | s' == "virtual" = pure VIRTUAL
      | s' == "when" = pure WHEN
      | s' == "where" = pure WHERE
      | s' == "window" = pure WINDOW
      | s' == "without" = pure WITHOUT
      | s' == "with" = pure WITH
      | otherwise = makeIdentifier s
      where
        s' :: Text
        s' =
          Text.toLower s

makeIdentifier :: Text -> TextParser Token
makeIdentifier s =
  if "sqlite_" `Text.isPrefixOf` s
    then fail "reserved identifier"
    else pure (Identifier s)
