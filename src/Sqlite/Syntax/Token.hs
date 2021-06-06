module Sqlite.Syntax.Token
  ( Token (..),
    render,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import GHC.Generics (Generic)
import Numeric.Natural
import Prelude

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
  | LeftParenthesis -- ^ @(@
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
  -- TODO split into integer/real
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
  | Parameter (Maybe Natural) -- ^ @?[NNNN]@
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
  | RightParenthesis -- ^ @)@
  | SAVEPOINT -- ^ @SAVEPOINT@
  | SELECT -- ^ @SELECT@
  | SET -- ^ @SET@
  | STORED -- ^ @STORED@
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

render :: Token -> Text.Builder
render = \case
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
  Blob s -> "x'" <> Text.Builder.fromText s <> "'"
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
  Identifier s -> Text.Builder.fromText s -- FIXME escape
  JOIN -> "JOIN"
  KEY -> "KEY"
  LAST -> "LAST"
  LEFT -> "LEFT"
  LIKE -> "LIKE"
  LIMIT -> "LIMIT"
  LeftParenthesis -> "("
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
  Number s -> Text.Builder.fromText s
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
  Parameter mn -> maybe id (\n -> (<> Text.Builder.fromString (show n))) mn "?"
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
  RightParenthesis -> ")"
  SAVEPOINT -> "SAVEPOINT"
  SELECT -> "SELECT"
  SET -> "SET"
  STORED -> "STORED"
  Semicolon -> ";"
  Solidus -> "/"
  String s -> "'" <> Text.Builder.fromText (Text.replace "'" "''" s) <> "'"
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
