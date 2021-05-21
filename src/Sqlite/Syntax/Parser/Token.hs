module Sqlite.Syntax.Parser.Token where

import Data.Text (Text)
import Sqlite.Syntax.Token (Token (..))
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, not, null)

type Parser r =
  Earley.Prod r Text Token

abort :: Parser r Token
abort =
  Earley.token ABORT

add :: Parser r Token
add =
  Earley.token ADD

all :: Parser r Token
all =
  Earley.token ALL

alter :: Parser r Token
alter =
  Earley.token ALTER

always :: Parser r Token
always =
  Earley.token ALWAYS

analyze :: Parser r Token
analyze =
  Earley.token ANALYZE

as :: Parser r Token
as =
  Earley.token AS

asc :: Parser r Token
asc =
  Earley.token ASC

attach :: Parser r Token
attach =
  Earley.token ATTACH

autoincrement :: Parser r Token
autoincrement =
  Earley.token AUTOINCREMENT

begin :: Parser r Token
begin =
  Earley.token BEGIN

blob :: Parser r Text
blob =
  Earley.terminal \case
    Blob s -> Just s
    _ -> Nothing

by :: Parser r Token
by =
  Earley.token BY

check :: Parser r Token
check =
  Earley.token CHECK

collate :: Parser r Token
collate =
  Earley.token COLLATE

column :: Parser r Token
column =
  Earley.token COLUMN

comma :: Parser r Token
comma =
  Earley.token Comma

commit :: Parser r Token
commit =
  Earley.token COMMIT

conflict :: Parser r Token
conflict =
  Earley.token CONFLICT

constraint :: Parser r Token
constraint =
  Earley.token CONSTRAINT

create :: Parser r Token
create =
  Earley.token CREATE

currentDate :: Parser r Token
currentDate =
  Earley.token CURRENT_DATE

currentTime :: Parser r Token
currentTime =
  Earley.token CURRENT_TIME

currentTimestamp :: Parser r Token
currentTimestamp =
  Earley.token CURRENT_TIMESTAMP

database :: Parser r Token
database =
  Earley.token DATABASE

deferred :: Parser r Token
deferred =
  Earley.token DEFERRED

desc :: Parser r Token
desc =
  Earley.token DESC

distinct :: Parser r Token
distinct =
  Earley.token DISTINCT

drop :: Parser r Token
drop =
  Earley.token DROP

end :: Parser r Token
end =
  Earley.token END

except :: Parser r Token
except =
  Earley.token EXCEPT

exclusive :: Parser r Token
exclusive =
  Earley.token EXCLUSIVE

exists :: Parser r Token
exists =
  Earley.token EXISTS

fail :: Parser r Token
fail =
  Earley.token FAIL

false :: Parser r Token
false =
  Earley.token FALSE

foreign_ :: Parser r Token
foreign_ =
  Earley.token FOREIGN

fullStop :: Parser r Token
fullStop =
  Earley.token FullStop

generated :: Parser r Token
generated =
  Earley.token GENERATED

group :: Parser r Token
group =
  Earley.token GROUP

having :: Parser r Token
having =
  Earley.token HAVING

hyphenMinus :: Parser r Token
hyphenMinus =
  Earley.token HyphenMinus

identifier :: Parser r Text
identifier =
  Earley.terminal \case
    Identifier s -> Just s
    _ -> Nothing

if_ :: Parser r Token
if_ =
  Earley.token IF

ignore :: Parser r Token
ignore =
  Earley.token IGNORE

immediate :: Parser r Token
immediate =
  Earley.token IMMEDIATE

index :: Parser r Token
index =
  Earley.token INDEX

intersect :: Parser r Token
intersect =
  Earley.token INTERSECT

key :: Parser r Token
key =
  Earley.token KEY

leftParenthesis :: Parser r Token
leftParenthesis =
  Earley.token LeftParenthesis

limit :: Parser r Token
limit =
  Earley.token LIMIT

not :: Parser r Token
not =
  Earley.token NOT

null :: Parser r Token
null =
  Earley.token NULL

number :: Parser r Text
number =
  Earley.terminal \case
    Number s -> Just s
    _ -> Nothing

offset :: Parser r Token
offset =
  Earley.token OFFSET

on :: Parser r Token
on =
  Earley.token ON

plusSign :: Parser r Token
plusSign =
  Earley.token PlusSign

primary :: Parser r Token
primary =
  Earley.token PRIMARY

recursive :: Parser r Token
recursive =
  Earley.token RECURSIVE

rename :: Parser r Token
rename =
  Earley.token RENAME

replace :: Parser r Token
replace =
  Earley.token REPLACE

rightParenthesis :: Parser r Token
rightParenthesis =
  Earley.token RightParenthesis

rollback :: Parser r Token
rollback =
  Earley.token ROLLBACK

rowid :: Parser r Token
rowid =
  Earley.token ROWID

savepoint :: Parser r Token
savepoint =
  Earley.token SAVEPOINT

select :: Parser r Token
select =
  Earley.token SELECT

stored :: Parser r Token
stored =
  Earley.token STORED

string :: Parser r Text
string =
  Earley.terminal \case
    String s -> Just s
    _ -> Nothing

table :: Parser r Token
table =
  Earley.token TABLE

to :: Parser r Token
to =
  Earley.token TO

temp :: Parser r Token
temp =
  Earley.token TEMP

temporary :: Parser r Token
temporary =
  Earley.token TEMPORARY

transaction :: Parser r Token
transaction =
  Earley.token TRANSACTION

true :: Parser r Token
true =
  Earley.token TRUE

union :: Parser r Token
union =
  Earley.token UNION

unique :: Parser r Token
unique =
  Earley.token UNIQUE

values :: Parser r Token
values =
  Earley.token VALUES

virtual :: Parser r Token
virtual =
  Earley.token VIRTUAL

where_ :: Parser r Token
where_ =
  Earley.token WHERE

with :: Parser r Token
with =
  Earley.token WITH

without :: Parser r Token
without =
  Earley.token WITHOUT
