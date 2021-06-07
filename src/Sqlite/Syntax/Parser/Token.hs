module Sqlite.Syntax.Parser.Token where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Sqlite.Syntax.Token (LocatedToken (..), Token (..))
import qualified Text.Earley as Earley
import Prelude hiding (Ordering, fail, not, null)

type Terminal r =
  Earley.Prod r Text LocatedToken

abort :: Terminal r ()
abort =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ABORT)

action :: Terminal r ()
action =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ACTION)

add :: Terminal r ()
add =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ADD)

all :: Terminal r ()
all =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ALL)

alter :: Terminal r ()
alter =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ALTER)

always :: Terminal r ()
always =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ALWAYS)

ampersand :: Terminal r ()
ampersand =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == Ampersand)

analyze :: Terminal r ()
analyze =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ANALYZE)

and :: Terminal r ()
and =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == AND)

as :: Terminal r ()
as =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == AS)

asc :: Terminal r ()
asc =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ASC)

asterisk :: Terminal r ()
asterisk =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == Asterisk)

attach :: Terminal r ()
attach =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ATTACH)

autoincrement :: Terminal r ()
autoincrement =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == AUTOINCREMENT)

begin :: Terminal r ()
begin =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == BEGIN)

between :: Terminal r ()
between =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == BETWEEN)

blob :: Terminal r Text
blob =
  Earley.terminal \case
    LocatedToken (Blob s) _ -> Just s
    _ -> Nothing

by :: Terminal r ()
by =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == BY)

cascade :: Terminal r ()
cascade =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CASCADE)

case_ :: Terminal r ()
case_ =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CASE)

cast :: Terminal r ()
cast =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CAST)

check :: Terminal r ()
check =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CHECK)

collate :: Terminal r ()
collate =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == COLLATE)

column :: Terminal r ()
column =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == COLUMN)

comma :: Terminal r ()
comma =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == Comma)

commit :: Terminal r ()
commit =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == COMMIT)

conflict :: Terminal r ()
conflict =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CONFLICT)

constraint :: Terminal r ()
constraint =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CONSTRAINT)

create :: Terminal r ()
create =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CREATE)

cross :: Terminal r ()
cross =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CROSS)

current :: Terminal r ()
current =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CURRENT)

currentDate :: Terminal r ()
currentDate =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CURRENT_DATE)

currentTime :: Terminal r ()
currentTime =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CURRENT_TIME)

currentTimestamp :: Terminal r ()
currentTimestamp =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == CURRENT_TIMESTAMP)

database :: Terminal r ()
database =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == DATABASE)

default_ :: Terminal r ()
default_ =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == DEFAULT)

deferrable :: Terminal r ()
deferrable =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == DEFERRABLE)

deferred :: Terminal r Token
deferred =
  Earley.terminal \(LocatedToken tok _) -> DEFERRED <$ guard (tok == DEFERRED)

delete :: Terminal r ()
delete =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == DELETE)

desc :: Terminal r ()
desc =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == DESC)

distinct :: Terminal r ()
distinct =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == DISTINCT)

drop :: Terminal r ()
drop =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == DROP)

else_ :: Terminal r ()
else_ =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ELSE)

end :: Terminal r ()
end =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == END)

equalsSign :: Terminal r ()
equalsSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == EqualsSign)

equalsSignEqualsSign :: Terminal r ()
equalsSignEqualsSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == EqualsSignEqualsSign)

escape :: Terminal r ()
escape =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ESCAPE)

except :: Terminal r ()
except =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == EXCEPT)

exclamationMarkEqualsSign :: Terminal r ()
exclamationMarkEqualsSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ExclamationMarkEqualsSign)

exclude :: Terminal r ()
exclude =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == EXCLUDE)

exclusive :: Terminal r ()
exclusive =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == EXCLUSIVE)

exists :: Terminal r ()
exists =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == EXISTS)

fail :: Terminal r ()
fail =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == FAIL)

false :: Terminal r ()
false =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == FALSE)

filter :: Terminal r ()
filter =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == FILTER)

first :: Terminal r ()
first =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == FIRST)

following :: Terminal r ()
following =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == FOLLOWING)

foreign_ :: Terminal r ()
foreign_ =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == FOREIGN)

from :: Terminal r ()
from =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == FROM)

fullStop :: Terminal r ()
fullStop =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == FullStop)

generated :: Terminal r ()
generated =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == GENERATED)

glob :: Terminal r ()
glob =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == GLOB)

greaterThanSign :: Terminal r ()
greaterThanSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == GreaterThanSign)

greaterThanSignEqualsSign :: Terminal r ()
greaterThanSignEqualsSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == GreaterThanSignEqualsSign)

greaterThanSignGreaterThanSign :: Terminal r ()
greaterThanSignGreaterThanSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == GreaterThanSignGreaterThanSign)

group :: Terminal r ()
group =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == GROUP)

groups :: Terminal r ()
groups =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == GROUPS)

having :: Terminal r ()
having =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == HAVING)

hyphenMinus :: Terminal r ()
hyphenMinus =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == HyphenMinus)

identifier :: Terminal r Text
identifier =
  Earley.terminal \case
    LocatedToken (Identifier s) _ -> Just s
    _ -> Nothing

if_ :: Terminal r ()
if_ =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == IF)

ignore :: Terminal r ()
ignore =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == IGNORE)

immediate :: Terminal r Token
immediate =
  Earley.terminal \(LocatedToken tok _) -> IMMEDIATE <$ guard (tok == IMMEDIATE)

in_ :: Terminal r ()
in_ =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == IN)

index :: Terminal r ()
index =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == INDEX)

indexed :: Terminal r ()
indexed =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == INDEXED)

initially :: Terminal r ()
initially =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == INITIALLY)

inner :: Terminal r ()
inner =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == INNER)

intersect :: Terminal r ()
intersect =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == INTERSECT)

is :: Terminal r ()
is =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == IS)

isnull :: Terminal r ()
isnull =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ISNULL)

join :: Terminal r ()
join =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == JOIN)

key :: Terminal r ()
key =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == KEY)

last :: Terminal r ()
last =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == LAST)

left :: Terminal r ()
left =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == LEFT)

leftParenthesis :: Terminal r ()
leftParenthesis =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == LeftParenthesis)

lessThanSign :: Terminal r ()
lessThanSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == LessThanSign)

lessThanSignEqualsSign :: Terminal r ()
lessThanSignEqualsSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == LessThanSignEqualsSign)

lessThanSignGreaterThanSign :: Terminal r ()
lessThanSignGreaterThanSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == LessThanSignGreaterThanSign)

lessThanSignLessThanSign :: Terminal r ()
lessThanSignLessThanSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == LessThanSignLessThanSign)

like :: Terminal r ()
like =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == LIKE)

limit :: Terminal r ()
limit =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == LIMIT)

match :: Terminal r ()
match =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == MATCH)

materialized :: Terminal r ()
materialized =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == MATERIALIZED)

namedParameter :: Terminal r Text
namedParameter =
  Earley.terminal \case
    LocatedToken (NamedParameter s) _ -> Just s
    _ -> Nothing

natural :: Terminal r ()
natural =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == NATURAL)

no :: Terminal r ()
no =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == NO)

not :: Terminal r ()
not =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == NOT)

notnull :: Terminal r ()
notnull =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == NOTNULL)

null :: Terminal r ()
null =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == NULL)

nulls :: Terminal r ()
nulls =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == NULLS)

number :: Terminal r Text
number =
  Earley.terminal \case
    LocatedToken (Number s) _ -> Just s
    _ -> Nothing

offset :: Terminal r ()
offset =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == OFFSET)

on :: Terminal r ()
on =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ON)

or :: Terminal r ()
or =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == OR)

order :: Terminal r ()
order =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ORDER)

others :: Terminal r ()
others =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == OTHERS)

outer :: Terminal r ()
outer =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == OUTER)

over :: Terminal r ()
over =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == OVER)

parameter :: Terminal r (Maybe Natural)
parameter =
  Earley.terminal \case
    LocatedToken (Parameter n) _ -> Just n
    _ -> Nothing

partition :: Terminal r ()
partition =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == PARTITION)

percentSign :: Terminal r ()
percentSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == PercentSign)

plusSign :: Terminal r ()
plusSign =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == PlusSign)

preceding :: Terminal r ()
preceding =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == PRECEDING)

primary :: Terminal r ()
primary =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == PRIMARY)

raise :: Terminal r ()
raise =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == RAISE)

range :: Terminal r ()
range =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == RANGE)

recursive :: Terminal r ()
recursive =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == RECURSIVE)

references :: Terminal r ()
references =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == REFERENCES)

regexp :: Terminal r ()
regexp =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == REGEXP)

rename :: Terminal r ()
rename =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == RENAME)

replace :: Terminal r ()
replace =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == REPLACE)

restrict :: Terminal r ()
restrict =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == RESTRICT)

rightParenthesis :: Terminal r ()
rightParenthesis =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == RightParenthesis)

rollback :: Terminal r ()
rollback =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ROLLBACK)

row :: Terminal r ()
row =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ROW)

rowid :: Terminal r ()
rowid = do
  Earley.terminal \case
    LocatedToken (Identifier s) _ | Text.toCaseFold s == "rowid" -> Just ()
    _ -> Nothing

rows :: Terminal r ()
rows =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == ROWS)

savepoint :: Terminal r ()
savepoint =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == SAVEPOINT)

select :: Terminal r ()
select =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == SELECT)

set :: Terminal r ()
set =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == SET)

solidus :: Terminal r ()
solidus =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == Solidus)

stored :: Terminal r ()
stored =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == STORED)

string :: Terminal r Text
string =
  Earley.terminal \case
    LocatedToken (String s) _ -> Just s
    _ -> Nothing

table :: Terminal r ()
table =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == TABLE)

then_ :: Terminal r ()
then_ =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == THEN)

ties :: Terminal r ()
ties =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == TIES)

to :: Terminal r ()
to =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == TO)

temp :: Terminal r ()
temp =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == TEMP)

temporary :: Terminal r ()
temporary =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == TEMPORARY)

tilde :: Terminal r ()
tilde =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == Tilde)

transaction :: Terminal r ()
transaction =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == TRANSACTION)

true :: Terminal r ()
true =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == TRUE)

unbounded :: Terminal r ()
unbounded =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == UNBOUNDED)

union :: Terminal r ()
union =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == UNION)

unique :: Terminal r ()
unique =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == UNIQUE)

update :: Terminal r ()
update =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == UPDATE)

using :: Terminal r ()
using =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == USING)

values :: Terminal r ()
values =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == VALUES)

verticalLine :: Terminal r ()
verticalLine =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == VerticalLine)

verticalLineVerticalLine :: Terminal r ()
verticalLineVerticalLine =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == VerticalLineVerticalLine)

virtual :: Terminal r ()
virtual =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == VIRTUAL)

when :: Terminal r ()
when =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == WHEN)

where_ :: Terminal r ()
where_ =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == WHERE)

window :: Terminal r ()
window =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == WINDOW)

with :: Terminal r ()
with =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == WITH)

without :: Terminal r ()
without =
  Earley.terminal \(LocatedToken tok _) -> guard (tok == WITHOUT)
