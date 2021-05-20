module Sqlite.Syntax.Parser.Token where

import Data.Text (Text)
import qualified Sqlite.Syntax.Lexer as Lexer
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, not, null)

type Parser r =
  Earley.Prod r Text Lexer.Token

abort :: Parser r Lexer.Token
abort =
  Earley.token Lexer.ABORT

add :: Parser r Lexer.Token
add =
  Earley.token Lexer.ADD

alter :: Parser r Lexer.Token
alter =
  Earley.token Lexer.ALTER

always :: Parser r Lexer.Token
always =
  Earley.token Lexer.ALWAYS

analyze :: Parser r Lexer.Token
analyze =
  Earley.token Lexer.ANALYZE

as :: Parser r Lexer.Token
as =
  Earley.token Lexer.AS

asc :: Parser r Lexer.Token
asc =
  Earley.token Lexer.ASC

attach :: Parser r Lexer.Token
attach =
  Earley.token Lexer.ATTACH

autoincrement :: Parser r Lexer.Token
autoincrement =
  Earley.token Lexer.AUTOINCREMENT

begin :: Parser r Lexer.Token
begin =
  Earley.token Lexer.BEGIN

blob :: Parser r Text
blob =
  Earley.terminal \case
    Lexer.Blob s -> Just s
    _ -> Nothing

check :: Parser r Lexer.Token
check =
  Earley.token Lexer.CHECK

collate :: Parser r Lexer.Token
collate =
  Earley.token Lexer.COLLATE

column :: Parser r Lexer.Token
column =
  Earley.token Lexer.COLUMN

conflict :: Parser r Lexer.Token
conflict =
  Earley.token Lexer.CONFLICT

constraint :: Parser r Lexer.Token
constraint =
  Earley.token Lexer.CONSTRAINT

currentDate :: Parser r Lexer.Token
currentDate =
  Earley.token Lexer.CURRENT_DATE

currentTime :: Parser r Lexer.Token
currentTime =
  Earley.token Lexer.CURRENT_TIME

currentTimestamp :: Parser r Lexer.Token
currentTimestamp =
  Earley.token Lexer.CURRENT_TIMESTAMP

database :: Parser r Lexer.Token
database =
  Earley.token Lexer.DATABASE

deferred :: Parser r Lexer.Token
deferred =
  Earley.token Lexer.DEFERRED

desc :: Parser r Lexer.Token
desc =
  Earley.token Lexer.DESC

drop :: Parser r Lexer.Token
drop =
  Earley.token Lexer.DROP

exclusive :: Parser r Lexer.Token
exclusive =
  Earley.token Lexer.EXCLUSIVE

fail :: Parser r Lexer.Token
fail =
  Earley.token Lexer.FAIL

false :: Parser r Lexer.Token
false =
  Earley.token Lexer.FALSE

fullStop :: Parser r Lexer.Token
fullStop =
  Earley.token Lexer.FullStop

generated :: Parser r Lexer.Token
generated =
  Earley.token Lexer.GENERATED

hyphenMinus :: Parser r Lexer.Token
hyphenMinus =
  Earley.token Lexer.HyphenMinus

identifier :: Parser r Text
identifier =
  Earley.terminal \case
    Lexer.Identifier s -> Just s
    _ -> Nothing

ignore :: Parser r Lexer.Token
ignore =
  Earley.token Lexer.IGNORE

immediate :: Parser r Lexer.Token
immediate =
  Earley.token Lexer.IMMEDIATE

key :: Parser r Lexer.Token
key =
  Earley.token Lexer.KEY

leftParenthesis :: Parser r Lexer.Token
leftParenthesis =
  Earley.token Lexer.LeftParenthesis

not :: Parser r Lexer.Token
not =
  Earley.token Lexer.NOT

null :: Parser r Lexer.Token
null =
  Earley.token Lexer.NULL

number :: Parser r Text
number =
  Earley.terminal \case
    Lexer.Number s -> Just s
    _ -> Nothing

on :: Parser r Lexer.Token
on =
  Earley.token Lexer.ON

plusSign :: Parser r Lexer.Token
plusSign =
  Earley.token Lexer.PlusSign

primary :: Parser r Lexer.Token
primary =
  Earley.token Lexer.PRIMARY

rename :: Parser r Lexer.Token
rename =
  Earley.token Lexer.RENAME

replace :: Parser r Lexer.Token
replace =
  Earley.token Lexer.REPLACE

rightParenthesis :: Parser r Lexer.Token
rightParenthesis =
  Earley.token Lexer.RightParenthesis

rollback :: Parser r Lexer.Token
rollback =
  Earley.token Lexer.ROLLBACK

stored :: Parser r Lexer.Token
stored =
  Earley.token Lexer.STORED

string :: Parser r Text
string =
  Earley.terminal \case
    Lexer.String s -> Just s
    _ -> Nothing

table :: Parser r Lexer.Token
table =
  Earley.token Lexer.TABLE

to :: Parser r Lexer.Token
to =
  Earley.token Lexer.TO

transaction :: Parser r Lexer.Token
transaction =
  Earley.token Lexer.TRANSACTION

true :: Parser r Lexer.Token
true =
  Earley.token Lexer.TRUE

unique :: Parser r Lexer.Token
unique =
  Earley.token Lexer.UNIQUE

virtual :: Parser r Lexer.Token
virtual =
  Earley.token Lexer.VIRTUAL
