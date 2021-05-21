module Main where

import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Sqlite.Syntax.Lexer (lex)
import Sqlite.Syntax.Token (Token (..))
import qualified Sqlite.Syntax.Token as Token (render)
import Prelude hiding (lex)

main :: IO Bool
main = do
  (check . withTests 10000 . property) do
    tokens0 <- forAll (Gen.list (Range.linear 1 10) genToken)
    let input0 =
          Text.Lazy.toStrict (Text.Builder.toLazyText (mconcat (List.intersperse " " (map Token.render tokens0))))
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
      pure LeftParenthesis,
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
      pure RightParenthesis,
      pure SAVEPOINT,
      pure SELECT,
      pure SET,
      pure STORED,
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
      Blob . elongateHexit <$> Gen.text (Range.linear 0 10) Gen.hexit,
      Identifier <$> Gen.element ["foo", "bar", "baz"]
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
