module Sqlite.Syntax.Parser.Token where

import Data.Text (Text)
import Numeric.Natural (Natural)
import Sqlite.Syntax.Token (Token (..))
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, not, null)

type Parser r =
  Earley.Prod r Text Token

abort :: Parser r Token
abort =
  Earley.token ABORT

action :: Parser r Token
action =
  Earley.token ACTION

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

ampersand :: Parser r Token
ampersand =
  Earley.token Ampersand

analyze :: Parser r Token
analyze =
  Earley.token ANALYZE

and :: Parser r Token
and =
  Earley.token AND

as :: Parser r Token
as =
  Earley.token AS

asc :: Parser r Token
asc =
  Earley.token ASC

asterisk :: Parser r Token
asterisk =
  Earley.token Asterisk

attach :: Parser r Token
attach =
  Earley.token ATTACH

autoincrement :: Parser r Token
autoincrement =
  Earley.token AUTOINCREMENT

begin :: Parser r Token
begin =
  Earley.token BEGIN

between :: Parser r Token
between =
  Earley.token BETWEEN

blob :: Parser r Text
blob =
  Earley.terminal \case
    Blob s -> Just s
    _ -> Nothing

by :: Parser r Token
by =
  Earley.token BY

cascade :: Parser r Token
cascade =
  Earley.token CASCADE

case_ :: Parser r Token
case_ =
  Earley.token CASE

cast :: Parser r Token
cast =
  Earley.token CAST

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

cross :: Parser r Token
cross =
  Earley.token CROSS

current :: Parser r Token
current =
  Earley.token CURRENT

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

default_ :: Parser r Token
default_ =
  Earley.token DEFAULT

deferrable :: Parser r Token
deferrable =
  Earley.token DEFERRABLE

deferred :: Parser r Token
deferred =
  Earley.token DEFERRED

delete :: Parser r Token
delete =
  Earley.token DELETE

desc :: Parser r Token
desc =
  Earley.token DESC

distinct :: Parser r Token
distinct =
  Earley.token DISTINCT

drop :: Parser r Token
drop =
  Earley.token DROP

else_ :: Parser r Token
else_ =
  Earley.token ELSE

end :: Parser r Token
end =
  Earley.token END

equalsSign :: Parser r Token
equalsSign =
  Earley.token EqualsSign

equalsSignEqualsSign :: Parser r Token
equalsSignEqualsSign =
  Earley.token EqualsSignEqualsSign

escape :: Parser r Token
escape =
  Earley.token ESCAPE

except :: Parser r Token
except =
  Earley.token EXCEPT

exclamationMarkEqualsSign :: Parser r Token
exclamationMarkEqualsSign =
  Earley.token ExclamationMarkEqualsSign

exclude :: Parser r Token
exclude =
  Earley.token EXCLUDE

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

filter :: Parser r Token
filter =
  Earley.token FILTER

first :: Parser r Token
first =
  Earley.token FIRST

following :: Parser r Token
following =
  Earley.token FOLLOWING

foreign_ :: Parser r Token
foreign_ =
  Earley.token FOREIGN

from :: Parser r Token
from =
  Earley.token FROM

fullStop :: Parser r Token
fullStop =
  Earley.token FullStop

generated :: Parser r Token
generated =
  Earley.token GENERATED

glob :: Parser r Token
glob =
  Earley.token GLOB

greaterThanSign :: Parser r Token
greaterThanSign =
  Earley.token GreaterThanSign

greaterThanSignEqualsSign :: Parser r Token
greaterThanSignEqualsSign =
  Earley.token GreaterThanSignEqualsSign

greaterThanSignGreaterThanSign :: Parser r Token
greaterThanSignGreaterThanSign =
  Earley.token GreaterThanSignGreaterThanSign

group :: Parser r Token
group =
  Earley.token GROUP

groups :: Parser r Token
groups =
  Earley.token GROUPS

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

in_ :: Parser r Token
in_ =
  Earley.token IN

index :: Parser r Token
index =
  Earley.token INDEX

indexed :: Parser r Token
indexed =
  Earley.token INDEXED

initially :: Parser r Token
initially =
  Earley.token INITIALLY

inner :: Parser r Token
inner =
  Earley.token INNER

intersect :: Parser r Token
intersect =
  Earley.token INTERSECT

is :: Parser r Token
is =
  Earley.token IS

isnull :: Parser r Token
isnull =
  Earley.token ISNULL

join :: Parser r Token
join =
  Earley.token JOIN

key :: Parser r Token
key =
  Earley.token KEY

last :: Parser r Token
last =
  Earley.token LAST

left :: Parser r Token
left =
  Earley.token LEFT

leftParenthesis :: Parser r Token
leftParenthesis =
  Earley.token LeftParenthesis

lessThanSign :: Parser r Token
lessThanSign =
  Earley.token LessThanSign

lessThanSignEqualsSign :: Parser r Token
lessThanSignEqualsSign =
  Earley.token LessThanSignEqualsSign

lessThanSignGreaterThanSign :: Parser r Token
lessThanSignGreaterThanSign =
  Earley.token LessThanSignGreaterThanSign

lessThanSignLessThanSign :: Parser r Token
lessThanSignLessThanSign =
  Earley.token LessThanSignLessThanSign

like :: Parser r Token
like =
  Earley.token LIKE

limit :: Parser r Token
limit =
  Earley.token LIMIT

match :: Parser r Token
match =
  Earley.token MATCH

materialized :: Parser r Token
materialized =
  Earley.token MATERIALIZED

namedParameter :: Parser r Text
namedParameter =
  Earley.terminal \case
    NamedParameter s -> Just s
    _ -> Nothing

natural :: Parser r Token
natural =
  Earley.token NATURAL

no :: Parser r Token
no =
  Earley.token NO

not :: Parser r Token
not =
  Earley.token NOT

notnull :: Parser r Token
notnull =
  Earley.token NOTNULL

null :: Parser r Token
null =
  Earley.token NULL

nulls :: Parser r Token
nulls =
  Earley.token NULLS

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

or :: Parser r Token
or =
  Earley.token OR

order :: Parser r Token
order =
  Earley.token ORDER

others :: Parser r Token
others =
  Earley.token OTHERS

outer :: Parser r Token
outer =
  Earley.token OUTER

over :: Parser r Token
over =
  Earley.token OVER

parameter :: Parser r (Maybe Natural)
parameter =
  Earley.terminal \case
    Parameter n -> Just n
    _ -> Nothing

partition :: Parser r Token
partition =
  Earley.token PARTITION

percentSign :: Parser r Token
percentSign =
  Earley.token PercentSign

plusSign :: Parser r Token
plusSign =
  Earley.token PlusSign

preceding :: Parser r Token
preceding =
  Earley.token PRECEDING

primary :: Parser r Token
primary =
  Earley.token PRIMARY

raise :: Parser r Token
raise =
  Earley.token RAISE

range :: Parser r Token
range =
  Earley.token RANGE

recursive :: Parser r Token
recursive =
  Earley.token RECURSIVE

references :: Parser r Token
references =
  Earley.token REFERENCES

regexp :: Parser r Token
regexp =
  Earley.token REGEXP

rename :: Parser r Token
rename =
  Earley.token RENAME

replace :: Parser r Token
replace =
  Earley.token REPLACE

restrict :: Parser r Token
restrict =
  Earley.token RESTRICT

rightParenthesis :: Parser r Token
rightParenthesis =
  Earley.token RightParenthesis

rollback :: Parser r Token
rollback =
  Earley.token ROLLBACK

row :: Parser r Token
row =
  Earley.token ROW

rowid :: Parser r Token
rowid =
  Earley.token ROWID

rows :: Parser r Token
rows =
  Earley.token ROWS

savepoint :: Parser r Token
savepoint =
  Earley.token SAVEPOINT

select :: Parser r Token
select =
  Earley.token SELECT

set :: Parser r Token
set =
  Earley.token SET

solidus :: Parser r Token
solidus =
  Earley.token Solidus

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

then_ :: Parser r Token
then_ =
  Earley.token THEN

ties :: Parser r Token
ties =
  Earley.token TIES

to :: Parser r Token
to =
  Earley.token TO

temp :: Parser r Token
temp =
  Earley.token TEMP

temporary :: Parser r Token
temporary =
  Earley.token TEMPORARY

tilde :: Parser r Token
tilde =
  Earley.token Tilde

transaction :: Parser r Token
transaction =
  Earley.token TRANSACTION

true :: Parser r Token
true =
  Earley.token TRUE

unbounded :: Parser r Token
unbounded =
  Earley.token UNBOUNDED

union :: Parser r Token
union =
  Earley.token UNION

unique :: Parser r Token
unique =
  Earley.token UNIQUE

update :: Parser r Token
update =
  Earley.token UPDATE

using :: Parser r Token
using =
  Earley.token USING

values :: Parser r Token
values =
  Earley.token VALUES

verticalLine :: Parser r Token
verticalLine =
  Earley.token VerticalLine

verticalLineVerticalLine :: Parser r Token
verticalLineVerticalLine =
  Earley.token VerticalLineVerticalLine

virtual :: Parser r Token
virtual =
  Earley.token VIRTUAL

when :: Parser r Token
when =
  Earley.token WHEN

where_ :: Parser r Token
where_ =
  Earley.token WHERE

window :: Parser r Token
window =
  Earley.token WINDOW

with :: Parser r Token
with =
  Earley.token WITH

without :: Parser r Token
without =
  Earley.token WITHOUT
