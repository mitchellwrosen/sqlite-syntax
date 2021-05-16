module Main where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lexer
import Prelude hiding (lex)

main :: IO Bool
main = do
  (check . withTests 10000 . property) do
    tokens0 <- forAll (Gen.list (Range.linear 1 10) genToken)
    let input0 = Text.unwords (map renderToken tokens0)
    case lex input0 of
      Left err -> fail (Text.unpack err)
      Right tokens1 -> tokens0 === tokens1

genToken :: Gen Token
genToken =
  Gen.choice
    [ pure ABORT,
      pure ACTION,
      pure ADD,
      pure AFTER,
      pure ALL,
      pure ALTER,
      pure ALWAYS,
      pure ANALYZE,
      pure AND,
      pure AS,
      pure ASC,
      pure ATTACH,
      pure AUTOINCREMENT,
      pure Ampersand,
      pure Asterisk,
      pure BEFORE,
      pure BEGIN,
      pure BETWEEN,
      pure BY,
      pure CASCADE,
      pure CASE,
      pure CAST,
      pure CHECK,
      pure COLLATE,
      pure COLUMN,
      pure COMMIT,
      pure CONFLICT,
      pure CONSTRAINT,
      pure CREATE,
      pure CROSS,
      pure CURRENT,
      pure CURRENT_DATE,
      pure CURRENT_TIME,
      pure CURRENT_TIMESTAMP,
      pure Comma,
      pure DATABASE,
      pure DEFAULT,
      pure DEFERRABLE,
      pure DEFERRED,
      pure DELETE,
      pure DESC,
      pure DETACH,
      pure DISTINCT,
      pure DO,
      pure DROP,
      pure EACH,
      pure ELSE,
      pure END,
      pure ESCAPE,
      pure EXCEPT,
      pure EXCLUDE,
      pure EXCLUSIVE,
      pure EXISTS,
      pure EXPLAIN,
      pure EqualsSign,
      pure EqualsSignEqualsSign,
      pure ExclamationMarkEqualsSign,
      pure FAIL,
      pure FALSE,
      pure FILTER,
      pure FIRST,
      pure FOLLOWING,
      pure FOR,
      pure FOREIGN,
      pure FROM,
      pure FULL,
      pure FullStop,
      pure GENERATED,
      pure GLOB,
      pure GROUP,
      pure GROUPS,
      pure GreaterThanSign,
      pure GreaterThanSignEqualsSign,
      pure GreaterThanSignGreaterThanSign,
      pure HAVING,
      pure HyphenMinus,
      pure IF,
      pure IGNORE,
      pure IMMEDIATE,
      pure IN,
      pure INDEX,
      pure INDEXED,
      pure INITIALLY,
      pure INNER,
      pure INSERT,
      pure INSTEAD,
      pure INTERSECT,
      pure INTO,
      pure IS,
      pure ISNULL,
      pure JOIN,
      pure KEY,
      pure LAST,
      pure LEFT,
      pure LIKE,
      pure LIMIT,
      pure LeftParen,
      pure LessThanSign,
      pure LessThanSignEqualsSign,
      pure LessThanSignGreaterThanSign,
      pure LessThanSignLessThanSign,
      pure MATCH,
      pure MATERIALIZED,
      pure NATURAL,
      pure NO,
      pure NOT,
      pure NOTHING,
      pure NOTNULL,
      pure NULL,
      pure NULLS,
      pure OF,
      pure OFFSET,
      pure ON,
      pure OR,
      pure ORDER,
      pure OTHERS,
      pure OUTER,
      pure OVER,
      pure PARTITION,
      pure PLAN,
      pure PRAGMA,
      pure PRECEDING,
      pure PRIMARY,
      pure PercentSign,
      pure PlusSign,
      pure QUERY,
      pure RAISE,
      pure RANGE,
      pure RECURSIVE,
      pure REFERENCES,
      pure REGEXP,
      pure REINDEX,
      pure RELEASE,
      pure RENAME,
      pure REPLACE,
      pure RESTRICT,
      pure RETURNING,
      pure RIGHT,
      pure ROLLBACK,
      pure ROW,
      pure ROWID,
      pure ROWS,
      pure RightParen,
      pure SAVEPOINT,
      pure SELECT,
      pure SET,
      pure Semicolon,
      pure Solidus,
      pure TABLE,
      pure TEMP,
      pure TEMPORARY,
      pure THEN,
      pure TIES,
      pure TO,
      pure TRANSACTION,
      pure TRIGGER,
      pure TRUE,
      pure Tilde,
      pure UNBOUNDED,
      pure UNION,
      pure UNIQUE,
      pure UPDATE,
      pure USING,
      pure VACUUM,
      pure VALUES,
      pure VIEW,
      pure VIRTUAL,
      pure VerticalLine,
      pure VerticalLineVerticalLine,
      pure WHEN,
      pure WHERE,
      pure WINDOW,
      pure WITH,
      pure WITHOUT,
      Number <$> genNumber,
      String <$> Gen.text (Range.linear 0 10) Gen.unicode,
      Blob . elongateHexit <$> Gen.text (Range.linear 0 10) Gen.hexit
      -- Identifier <$> undefined,
    ]
  where
    genNumber :: Gen Text
    genNumber =
      Gen.choice
        [ (\s0 s1 s2 -> s0 <> fromMaybe "" s1 <> s2)
            <$> genDigits <*> Gen.maybe genFractional <*> genExponent,
          (\s0 s1 -> s0 <> s1) <$> genFractional <*> genExponent,
          (\s0 s1 -> s0 <> Text.pack s1)
            <$> Gen.element ["0x", "0X"]
            <*> Gen.list (Range.linear 1 10) Gen.hexit
        ]
      where
        genDigits :: Gen Text
        genDigits =
          Text.pack <$> Gen.list (Range.linear 1 10) Gen.digit

        genExponent :: Gen Text
        genExponent =
          (fmap (fromMaybe "") . Gen.maybe)
            ( (\s0 s1 s2 -> Text.singleton s0 <> maybe Text.empty Text.singleton s1 <> s2)
                <$> Gen.element ['e', 'E'] <*> Gen.maybe (Gen.element ['+', '-']) <*> genDigits
            )

        genFractional :: Gen Text
        genFractional =
          Text.cons '.' <$> genDigits

    elongateHexit :: Text -> Text
    elongateHexit s =
      if even (Text.length s)
        then s
        else Text.cons '0' s

-- Gen.int64 (Range.constant minBound maxBound),

renderToken :: Token -> Text
renderToken = \case
  ABORT -> "ABORT"
  ACTION -> "ACTION"
  ADD -> "ADD"
  AFTER -> "AFTER"
  ALL -> "ALL"
  ALTER -> "ALTER"
  ALWAYS -> "ALWAYS"
  ANALYZE -> "ANALYZE"
  AND -> "AND"
  AS -> "AS"
  ASC -> "ASC"
  ATTACH -> "ATTACH"
  AUTOINCREMENT -> "AUTOINCREMENT"
  Ampersand -> "&"
  Asterisk -> "*"
  BEFORE -> "BEFORE"
  BEGIN -> "BEGIN"
  BETWEEN -> "BETWEEN"
  BY -> "BY"
  Blob s -> Text.Lazy.toStrict (Text.Builder.toLazyText ("x'" <> Text.Builder.fromText s <> "'"))
  CASCADE -> "CASCADE"
  CASE -> "CASE"
  CAST -> "CAST"
  CHECK -> "CHECK"
  COLLATE -> "COLLATE"
  COLUMN -> "COLUMN"
  COMMIT -> "COMMIT"
  CONFLICT -> "CONFLICT"
  CONSTRAINT -> "CONSTRAINT"
  CREATE -> "CREATE"
  CROSS -> "CROSS"
  CURRENT -> "CURRENT"
  CURRENT_DATE -> "CURRENT_DATE"
  CURRENT_TIME -> "CURRENT_TIME"
  CURRENT_TIMESTAMP -> "CURRENT_TIMESTAMP"
  Comma -> ","
  DATABASE -> "DATABASE"
  DEFAULT -> "DEFAULT"
  DEFERRABLE -> "DEFERRABLE"
  DEFERRED -> "DEFERRED"
  DELETE -> "DELETE"
  DESC -> "DESC"
  DETACH -> "DETACH"
  DISTINCT -> "DISTINCT"
  DO -> "DO"
  DROP -> "DROP"
  EACH -> "EACH"
  ELSE -> "ELSE"
  END -> "END"
  ESCAPE -> "ESCAPE"
  EXCEPT -> "EXCEPT"
  EXCLUDE -> "EXCLUDE"
  EXCLUSIVE -> "EXCLUSIVE"
  EXISTS -> "EXISTS"
  EXPLAIN -> "EXPLAIN"
  EqualsSign -> "="
  EqualsSignEqualsSign -> "=="
  ExclamationMarkEqualsSign -> "!="
  FAIL -> "FAIL"
  FALSE -> "FALSE"
  FILTER -> "FILTER"
  FIRST -> "FIRST"
  FOLLOWING -> "FOLLOWING"
  FOR -> "FOR"
  FOREIGN -> "FOREIGN"
  FROM -> "FROM"
  FULL -> "FULL"
  FullStop -> "."
  GENERATED -> "GENERATED"
  GLOB -> "GLOB"
  GROUP -> "GROUP"
  GROUPS -> "GROUPS"
  GreaterThanSign -> ">"
  GreaterThanSignEqualsSign -> ">="
  GreaterThanSignGreaterThanSign -> ">>"
  HAVING -> "HAVING"
  HyphenMinus -> "-"
  IF -> "IF"
  IGNORE -> "IGNORE"
  IMMEDIATE -> "IMMEDIATE"
  IN -> "IN"
  INDEX -> "INDEX"
  INDEXED -> "INDEXED"
  INITIALLY -> "INITIALLY"
  INNER -> "INNER"
  INSERT -> "INSERT"
  INSTEAD -> "INSTEAD"
  INTERSECT -> "INTERSECT"
  INTO -> "INTO"
  IS -> "IS"
  ISNULL -> "ISNULL"
  Identifier _ -> "TODO"
  JOIN -> "JOIN"
  KEY -> "KEY"
  LAST -> "LAST"
  LEFT -> "LEFT"
  LIKE -> "LIKE"
  LIMIT -> "LIMIT"
  LeftParen -> "("
  LessThanSign -> "<"
  LessThanSignEqualsSign -> "<="
  LessThanSignGreaterThanSign -> "<>"
  LessThanSignLessThanSign -> "<<"
  MATCH -> "MATCH"
  MATERIALIZED -> "MATERIALIZED"
  NATURAL -> "NATURAL"
  NO -> "NO"
  NOT -> "NOT"
  NOTHING -> "NOTHING"
  NOTNULL -> "NOTNULL"
  NULL -> "NULL"
  NULLS -> "NULLS"
  Number s -> s
  OF -> "OF"
  OFFSET -> "OFFSET"
  ON -> "ON"
  OR -> "OR"
  ORDER -> "ORDER"
  OTHERS -> "OTHERS"
  OUTER -> "OUTER"
  OVER -> "OVER"
  PARTITION -> "PARTITION"
  PLAN -> "PLAN"
  PRAGMA -> "PRAGMA"
  PRECEDING -> "PRECEDING"
  PRIMARY -> "PRIMARY"
  PercentSign -> "%"
  PlusSign -> "+"
  QUERY -> "QUERY"
  RAISE -> "RAISE"
  RANGE -> "RANGE"
  RECURSIVE -> "RECURSIVE"
  REFERENCES -> "REFERENCES"
  REGEXP -> "REGEXP"
  REINDEX -> "REINDEX"
  RELEASE -> "RELEASE"
  RENAME -> "RENAME"
  REPLACE -> "REPLACE"
  RESTRICT -> "RESTRICT"
  RETURNING -> "RETURNING"
  RIGHT -> "RIGHT"
  ROLLBACK -> "ROLLBACK"
  ROW -> "ROW"
  ROWID -> "ROWID"
  ROWS -> "ROWS"
  RightParen -> ")"
  SAVEPOINT -> "SAVEPOINT"
  SELECT -> "SELECT"
  SET -> "SET"
  Semicolon -> ";"
  Solidus -> "/"
  String s ->
    Text.Lazy.toStrict (Text.Builder.toLazyText ("'" <> Text.Builder.fromText (Text.replace "'" "''" s) <> "'"))
  TABLE -> "TABLE"
  TEMP -> "TEMP"
  TEMPORARY -> "TEMPORARY"
  THEN -> "THEN"
  TIES -> "TIES"
  TO -> "TO"
  TRANSACTION -> "TRANSACTION"
  TRIGGER -> "TRIGGER"
  TRUE -> "TRUE"
  Tilde -> "~"
  UNBOUNDED -> "UNBOUNDED"
  UNION -> "UNION"
  UNIQUE -> "UNIQUE"
  UPDATE -> "UPDATE"
  USING -> "USING"
  VACUUM -> "VACUUM"
  VALUES -> "VALUES"
  VIEW -> "VIEW"
  VIRTUAL -> "VIRTUAL"
  VerticalLine -> "|"
  VerticalLineVerticalLine -> "||"
  WHEN -> "WHEN"
  WHERE -> "WHERE"
  WINDOW -> "WINDOW"
  WITH -> "WITH"
  WITHOUT -> "WITHOUT"
