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

all :: Parser r Lexer.Token
all =
  Earley.token Lexer.ALL

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

by :: Parser r Lexer.Token
by =
  Earley.token Lexer.BY

check :: Parser r Lexer.Token
check =
  Earley.token Lexer.CHECK

collate :: Parser r Lexer.Token
collate =
  Earley.token Lexer.COLLATE

column :: Parser r Lexer.Token
column =
  Earley.token Lexer.COLUMN

comma :: Parser r Lexer.Token
comma =
  Earley.token Lexer.Comma

commit :: Parser r Lexer.Token
commit =
  Earley.token Lexer.COMMIT

conflict :: Parser r Lexer.Token
conflict =
  Earley.token Lexer.CONFLICT

constraint :: Parser r Lexer.Token
constraint =
  Earley.token Lexer.CONSTRAINT

create :: Parser r Lexer.Token
create =
  Earley.token Lexer.CREATE

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

distinct :: Parser r Lexer.Token
distinct =
  Earley.token Lexer.DISTINCT

drop :: Parser r Lexer.Token
drop =
  Earley.token Lexer.DROP

end :: Parser r Lexer.Token
end =
  Earley.token Lexer.END

except :: Parser r Lexer.Token
except =
  Earley.token Lexer.EXCEPT

exclusive :: Parser r Lexer.Token
exclusive =
  Earley.token Lexer.EXCLUSIVE

exists :: Parser r Lexer.Token
exists =
  Earley.token Lexer.EXISTS

fail :: Parser r Lexer.Token
fail =
  Earley.token Lexer.FAIL

false :: Parser r Lexer.Token
false =
  Earley.token Lexer.FALSE

foreign_ :: Parser r Lexer.Token
foreign_ =
  Earley.token Lexer.FOREIGN

fullStop :: Parser r Lexer.Token
fullStop =
  Earley.token Lexer.FullStop

generated :: Parser r Lexer.Token
generated =
  Earley.token Lexer.GENERATED

group :: Parser r Lexer.Token
group =
  Earley.token Lexer.GROUP

having :: Parser r Lexer.Token
having =
  Earley.token Lexer.HAVING

hyphenMinus :: Parser r Lexer.Token
hyphenMinus =
  Earley.token Lexer.HyphenMinus

identifier :: Parser r Text
identifier =
  Earley.terminal \case
    Lexer.Identifier s -> Just s
    _ -> Nothing

if_ :: Parser r Lexer.Token
if_ =
  Earley.token Lexer.IF

ignore :: Parser r Lexer.Token
ignore =
  Earley.token Lexer.IGNORE

immediate :: Parser r Lexer.Token
immediate =
  Earley.token Lexer.IMMEDIATE

index :: Parser r Lexer.Token
index =
  Earley.token Lexer.INDEX

intersect :: Parser r Lexer.Token
intersect =
  Earley.token Lexer.INTERSECT

key :: Parser r Lexer.Token
key =
  Earley.token Lexer.KEY

leftParenthesis :: Parser r Lexer.Token
leftParenthesis =
  Earley.token Lexer.LeftParenthesis

limit :: Parser r Lexer.Token
limit =
  Earley.token Lexer.LIMIT

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

offset :: Parser r Lexer.Token
offset =
  Earley.token Lexer.OFFSET

on :: Parser r Lexer.Token
on =
  Earley.token Lexer.ON

plusSign :: Parser r Lexer.Token
plusSign =
  Earley.token Lexer.PlusSign

primary :: Parser r Lexer.Token
primary =
  Earley.token Lexer.PRIMARY

recursive :: Parser r Lexer.Token
recursive =
  Earley.token Lexer.RECURSIVE

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

rowid :: Parser r Lexer.Token
rowid =
  Earley.token Lexer.ROWID

savepoint :: Parser r Lexer.Token
savepoint =
  Earley.token Lexer.SAVEPOINT

select :: Parser r Lexer.Token
select =
  Earley.token Lexer.SELECT

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

temp :: Parser r Lexer.Token
temp =
  Earley.token Lexer.TEMP

temporary :: Parser r Lexer.Token
temporary =
  Earley.token Lexer.TEMPORARY

transaction :: Parser r Lexer.Token
transaction =
  Earley.token Lexer.TRANSACTION

true :: Parser r Lexer.Token
true =
  Earley.token Lexer.TRUE

union :: Parser r Lexer.Token
union =
  Earley.token Lexer.UNION

unique :: Parser r Lexer.Token
unique =
  Earley.token Lexer.UNIQUE

values :: Parser r Lexer.Token
values =
  Earley.token Lexer.VALUES

virtual :: Parser r Lexer.Token
virtual =
  Earley.token Lexer.VIRTUAL

where_ :: Parser r Lexer.Token
where_ =
  Earley.token Lexer.WHERE

with :: Parser r Lexer.Token
with =
  Earley.token Lexer.WITH

without :: Parser r Lexer.Token
without =
  Earley.token Lexer.WITHOUT
