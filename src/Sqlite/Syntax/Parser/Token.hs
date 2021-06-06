module Sqlite.Syntax.Parser.Token where

import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Sqlite.Syntax.Token (Token (..))
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, not, null)

type Terminal r =
  Earley.Prod r Text Token

abort :: Terminal r Token
abort =
  Earley.token ABORT

action :: Terminal r Token
action =
  Earley.token ACTION

add :: Terminal r Token
add =
  Earley.token ADD

all :: Terminal r Token
all =
  Earley.token ALL

alter :: Terminal r Token
alter =
  Earley.token ALTER

always :: Terminal r Token
always =
  Earley.token ALWAYS

ampersand :: Terminal r Token
ampersand =
  Earley.token Ampersand

analyze :: Terminal r Token
analyze =
  Earley.token ANALYZE

and :: Terminal r Token
and =
  Earley.token AND

as :: Terminal r Token
as =
  Earley.token AS

asc :: Terminal r Token
asc =
  Earley.token ASC

asterisk :: Terminal r Token
asterisk =
  Earley.token Asterisk

attach :: Terminal r Token
attach =
  Earley.token ATTACH

autoincrement :: Terminal r Token
autoincrement =
  Earley.token AUTOINCREMENT

begin :: Terminal r Token
begin =
  Earley.token BEGIN

between :: Terminal r Token
between =
  Earley.token BETWEEN

blob :: Terminal r Text
blob =
  Earley.terminal \case
    Blob s -> Just s
    _ -> Nothing

by :: Terminal r Token
by =
  Earley.token BY

cascade :: Terminal r Token
cascade =
  Earley.token CASCADE

case_ :: Terminal r Token
case_ =
  Earley.token CASE

cast :: Terminal r Token
cast =
  Earley.token CAST

check :: Terminal r Token
check =
  Earley.token CHECK

collate :: Terminal r Token
collate =
  Earley.token COLLATE

column :: Terminal r Token
column =
  Earley.token COLUMN

comma :: Terminal r Token
comma =
  Earley.token Comma

commit :: Terminal r Token
commit =
  Earley.token COMMIT

conflict :: Terminal r Token
conflict =
  Earley.token CONFLICT

constraint :: Terminal r Token
constraint =
  Earley.token CONSTRAINT

create :: Terminal r Token
create =
  Earley.token CREATE

cross :: Terminal r Token
cross =
  Earley.token CROSS

current :: Terminal r Token
current =
  Earley.token CURRENT

currentDate :: Terminal r Token
currentDate =
  Earley.token CURRENT_DATE

currentTime :: Terminal r Token
currentTime =
  Earley.token CURRENT_TIME

currentTimestamp :: Terminal r Token
currentTimestamp =
  Earley.token CURRENT_TIMESTAMP

database :: Terminal r Token
database =
  Earley.token DATABASE

default_ :: Terminal r Token
default_ =
  Earley.token DEFAULT

deferrable :: Terminal r Token
deferrable =
  Earley.token DEFERRABLE

deferred :: Terminal r Token
deferred =
  Earley.token DEFERRED

delete :: Terminal r Token
delete =
  Earley.token DELETE

desc :: Terminal r Token
desc =
  Earley.token DESC

distinct :: Terminal r Token
distinct =
  Earley.token DISTINCT

drop :: Terminal r Token
drop =
  Earley.token DROP

else_ :: Terminal r Token
else_ =
  Earley.token ELSE

end :: Terminal r Token
end =
  Earley.token END

equalsSign :: Terminal r Token
equalsSign =
  Earley.token EqualsSign

equalsSignEqualsSign :: Terminal r Token
equalsSignEqualsSign =
  Earley.token EqualsSignEqualsSign

escape :: Terminal r Token
escape =
  Earley.token ESCAPE

except :: Terminal r Token
except =
  Earley.token EXCEPT

exclamationMarkEqualsSign :: Terminal r Token
exclamationMarkEqualsSign =
  Earley.token ExclamationMarkEqualsSign

exclude :: Terminal r Token
exclude =
  Earley.token EXCLUDE

exclusive :: Terminal r Token
exclusive =
  Earley.token EXCLUSIVE

exists :: Terminal r Token
exists =
  Earley.token EXISTS

fail :: Terminal r Token
fail =
  Earley.token FAIL

false :: Terminal r Token
false =
  Earley.token FALSE

filter :: Terminal r Token
filter =
  Earley.token FILTER

first :: Terminal r Token
first =
  Earley.token FIRST

following :: Terminal r Token
following =
  Earley.token FOLLOWING

foreign_ :: Terminal r Token
foreign_ =
  Earley.token FOREIGN

from :: Terminal r Token
from =
  Earley.token FROM

fullStop :: Terminal r Token
fullStop =
  Earley.token FullStop

generated :: Terminal r Token
generated =
  Earley.token GENERATED

glob :: Terminal r Token
glob =
  Earley.token GLOB

greaterThanSign :: Terminal r Token
greaterThanSign =
  Earley.token GreaterThanSign

greaterThanSignEqualsSign :: Terminal r Token
greaterThanSignEqualsSign =
  Earley.token GreaterThanSignEqualsSign

greaterThanSignGreaterThanSign :: Terminal r Token
greaterThanSignGreaterThanSign =
  Earley.token GreaterThanSignGreaterThanSign

group :: Terminal r Token
group =
  Earley.token GROUP

groups :: Terminal r Token
groups =
  Earley.token GROUPS

having :: Terminal r Token
having =
  Earley.token HAVING

hyphenMinus :: Terminal r Token
hyphenMinus =
  Earley.token HyphenMinus

identifier :: Terminal r Text
identifier =
  Earley.terminal \case
    Identifier s -> Just s
    _ -> Nothing

if_ :: Terminal r Token
if_ =
  Earley.token IF

ignore :: Terminal r Token
ignore =
  Earley.token IGNORE

immediate :: Terminal r Token
immediate =
  Earley.token IMMEDIATE

in_ :: Terminal r Token
in_ =
  Earley.token IN

index :: Terminal r Token
index =
  Earley.token INDEX

indexed :: Terminal r Token
indexed =
  Earley.token INDEXED

initially :: Terminal r Token
initially =
  Earley.token INITIALLY

inner :: Terminal r Token
inner =
  Earley.token INNER

intersect :: Terminal r Token
intersect =
  Earley.token INTERSECT

is :: Terminal r Token
is =
  Earley.token IS

isnull :: Terminal r Token
isnull =
  Earley.token ISNULL

join :: Terminal r Token
join =
  Earley.token JOIN

key :: Terminal r Token
key =
  Earley.token KEY

last :: Terminal r Token
last =
  Earley.token LAST

left :: Terminal r Token
left =
  Earley.token LEFT

leftParenthesis :: Terminal r Token
leftParenthesis =
  Earley.token LeftParenthesis

lessThanSign :: Terminal r Token
lessThanSign =
  Earley.token LessThanSign

lessThanSignEqualsSign :: Terminal r Token
lessThanSignEqualsSign =
  Earley.token LessThanSignEqualsSign

lessThanSignGreaterThanSign :: Terminal r Token
lessThanSignGreaterThanSign =
  Earley.token LessThanSignGreaterThanSign

lessThanSignLessThanSign :: Terminal r Token
lessThanSignLessThanSign =
  Earley.token LessThanSignLessThanSign

like :: Terminal r Token
like =
  Earley.token LIKE

limit :: Terminal r Token
limit =
  Earley.token LIMIT

match :: Terminal r Token
match =
  Earley.token MATCH

materialized :: Terminal r Token
materialized =
  Earley.token MATERIALIZED

namedParameter :: Terminal r Text
namedParameter =
  Earley.terminal \case
    NamedParameter s -> Just s
    _ -> Nothing

natural :: Terminal r Token
natural =
  Earley.token NATURAL

no :: Terminal r Token
no =
  Earley.token NO

not :: Terminal r Token
not =
  Earley.token NOT

notnull :: Terminal r Token
notnull =
  Earley.token NOTNULL

null :: Terminal r Token
null =
  Earley.token NULL

nulls :: Terminal r Token
nulls =
  Earley.token NULLS

number :: Terminal r Text
number =
  Earley.terminal \case
    Number s -> Just s
    _ -> Nothing

offset :: Terminal r Token
offset =
  Earley.token OFFSET

on :: Terminal r Token
on =
  Earley.token ON

or :: Terminal r Token
or =
  Earley.token OR

order :: Terminal r Token
order =
  Earley.token ORDER

others :: Terminal r Token
others =
  Earley.token OTHERS

outer :: Terminal r Token
outer =
  Earley.token OUTER

over :: Terminal r Token
over =
  Earley.token OVER

parameter :: Terminal r (Maybe Natural)
parameter =
  Earley.terminal \case
    Parameter n -> Just n
    _ -> Nothing

partition :: Terminal r Token
partition =
  Earley.token PARTITION

percentSign :: Terminal r Token
percentSign =
  Earley.token PercentSign

plusSign :: Terminal r Token
plusSign =
  Earley.token PlusSign

preceding :: Terminal r Token
preceding =
  Earley.token PRECEDING

primary :: Terminal r Token
primary =
  Earley.token PRIMARY

raise :: Terminal r Token
raise =
  Earley.token RAISE

range :: Terminal r Token
range =
  Earley.token RANGE

recursive :: Terminal r Token
recursive =
  Earley.token RECURSIVE

references :: Terminal r Token
references =
  Earley.token REFERENCES

regexp :: Terminal r Token
regexp =
  Earley.token REGEXP

rename :: Terminal r Token
rename =
  Earley.token RENAME

replace :: Terminal r Token
replace =
  Earley.token REPLACE

restrict :: Terminal r Token
restrict =
  Earley.token RESTRICT

rightParenthesis :: Terminal r Token
rightParenthesis =
  Earley.token RightParenthesis

rollback :: Terminal r Token
rollback =
  Earley.token ROLLBACK

row :: Terminal r Token
row =
  Earley.token ROW

rowid :: Terminal r ()
rowid = do
  Earley.terminal \case
    Identifier s | Text.toCaseFold s == "rowid" -> Just ()
    _ -> Nothing

rows :: Terminal r Token
rows =
  Earley.token ROWS

savepoint :: Terminal r Token
savepoint =
  Earley.token SAVEPOINT

select :: Terminal r Token
select =
  Earley.token SELECT

set :: Terminal r Token
set =
  Earley.token SET

solidus :: Terminal r Token
solidus =
  Earley.token Solidus

stored :: Terminal r Token
stored =
  Earley.token STORED

string :: Terminal r Text
string =
  Earley.terminal \case
    String s -> Just s
    _ -> Nothing

table :: Terminal r Token
table =
  Earley.token TABLE

then_ :: Terminal r Token
then_ =
  Earley.token THEN

ties :: Terminal r Token
ties =
  Earley.token TIES

to :: Terminal r Token
to =
  Earley.token TO

temp :: Terminal r Token
temp =
  Earley.token TEMP

temporary :: Terminal r Token
temporary =
  Earley.token TEMPORARY

tilde :: Terminal r Token
tilde =
  Earley.token Tilde

transaction :: Terminal r Token
transaction =
  Earley.token TRANSACTION

true :: Terminal r Token
true =
  Earley.token TRUE

unbounded :: Terminal r Token
unbounded =
  Earley.token UNBOUNDED

union :: Terminal r Token
union =
  Earley.token UNION

unique :: Terminal r Token
unique =
  Earley.token UNIQUE

update :: Terminal r Token
update =
  Earley.token UPDATE

using :: Terminal r Token
using =
  Earley.token USING

values :: Terminal r Token
values =
  Earley.token VALUES

verticalLine :: Terminal r Token
verticalLine =
  Earley.token VerticalLine

verticalLineVerticalLine :: Terminal r Token
verticalLineVerticalLine =
  Earley.token VerticalLineVerticalLine

virtual :: Terminal r Token
virtual =
  Earley.token VIRTUAL

when :: Terminal r Token
when =
  Earley.token WHEN

where_ :: Terminal r Token
where_ =
  Earley.token WHERE

window :: Terminal r Token
window =
  Earley.token WINDOW

with :: Terminal r Token
with =
  Earley.token WITH

without :: Terminal r Token
without =
  Earley.token WITHOUT
