module Sqlite.Syntax.Parser.Token where

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
  Earley.terminal \case
    LocatedToken ABORT _ -> Just ()
    _ -> Nothing

action :: Terminal r ()
action =
  Earley.terminal \case
    LocatedToken ACTION _ -> Just ()
    _ -> Nothing

add :: Terminal r ()
add =
  Earley.terminal \case
    LocatedToken ADD _ -> Just ()
    _ -> Nothing

all :: Terminal r ()
all =
  Earley.terminal \case
    LocatedToken ALL _ -> Just ()
    _ -> Nothing

alter :: Terminal r ()
alter =
  Earley.terminal \case
    LocatedToken ALTER _ -> Just ()
    _ -> Nothing

always :: Terminal r ()
always =
  Earley.terminal \case
    LocatedToken ALWAYS _ -> Just ()
    _ -> Nothing

ampersand :: Terminal r ()
ampersand =
  Earley.terminal \case
    LocatedToken Ampersand _ -> Just ()
    _ -> Nothing

analyze :: Terminal r ()
analyze =
  Earley.terminal \case
    LocatedToken ANALYZE _ -> Just ()
    _ -> Nothing

and :: Terminal r ()
and =
  Earley.terminal \case
    LocatedToken AND _ -> Just ()
    _ -> Nothing

as :: Terminal r ()
as =
  Earley.terminal \case
    LocatedToken AS _ -> Just ()
    _ -> Nothing

asc :: Terminal r ()
asc =
  Earley.terminal \case
    LocatedToken ASC _ -> Just ()
    _ -> Nothing

asterisk :: Terminal r ()
asterisk =
  Earley.terminal \case
    LocatedToken Asterisk _ -> Just ()
    _ -> Nothing

attach :: Terminal r ()
attach =
  Earley.terminal \case
    LocatedToken ATTACH _ -> Just ()
    _ -> Nothing

autoincrement :: Terminal r ()
autoincrement =
  Earley.terminal \case
    LocatedToken AUTOINCREMENT _ -> Just ()
    _ -> Nothing

begin :: Terminal r ()
begin =
  Earley.terminal \case
    LocatedToken BEGIN _ -> Just ()
    _ -> Nothing

between :: Terminal r ()
between =
  Earley.terminal \case
    LocatedToken BETWEEN _ -> Just ()
    _ -> Nothing

blob :: Terminal r Text
blob =
  Earley.terminal \case
    LocatedToken (Blob s) _ -> Just s
    _ -> Nothing

by :: Terminal r ()
by =
  Earley.terminal \case
    LocatedToken BY _ -> Just ()
    _ -> Nothing

cascade :: Terminal r ()
cascade =
  Earley.terminal \case
    LocatedToken CASCADE _ -> Just ()
    _ -> Nothing

case_ :: Terminal r ()
case_ =
  Earley.terminal \case
    LocatedToken CASE _ -> Just ()
    _ -> Nothing

cast :: Terminal r ()
cast =
  Earley.terminal \case
    LocatedToken CAST _ -> Just ()
    _ -> Nothing

check :: Terminal r ()
check =
  Earley.terminal \case
    LocatedToken CHECK _ -> Just ()
    _ -> Nothing

collate :: Terminal r ()
collate =
  Earley.terminal \case
    LocatedToken COLLATE _ -> Just ()
    _ -> Nothing

column :: Terminal r ()
column =
  Earley.terminal \case
    LocatedToken COLUMN _ -> Just ()
    _ -> Nothing

comma :: Terminal r ()
comma =
  Earley.terminal \case
    LocatedToken Comma _ -> Just ()
    _ -> Nothing

commit :: Terminal r ()
commit =
  Earley.terminal \case
    LocatedToken COMMIT _ -> Just ()
    _ -> Nothing

conflict :: Terminal r ()
conflict =
  Earley.terminal \case
    LocatedToken CONFLICT _ -> Just ()
    _ -> Nothing

constraint :: Terminal r ()
constraint =
  Earley.terminal \case
    LocatedToken CONSTRAINT _ -> Just ()
    _ -> Nothing

create :: Terminal r ()
create =
  Earley.terminal \case
    LocatedToken CREATE _ -> Just ()
    _ -> Nothing

cross :: Terminal r ()
cross =
  Earley.terminal \case
    LocatedToken CROSS _ -> Just ()
    _ -> Nothing

current :: Terminal r ()
current =
  Earley.terminal \case
    LocatedToken CURRENT _ -> Just ()
    _ -> Nothing

currentDate :: Terminal r ()
currentDate =
  Earley.terminal \case
    LocatedToken CURRENT_DATE _ -> Just ()
    _ -> Nothing

currentTime :: Terminal r ()
currentTime =
  Earley.terminal \case
    LocatedToken CURRENT_TIME _ -> Just ()
    _ -> Nothing

currentTimestamp :: Terminal r ()
currentTimestamp =
  Earley.terminal \case
    LocatedToken CURRENT_TIMESTAMP _ -> Just ()
    _ -> Nothing

database :: Terminal r ()
database =
  Earley.terminal \case
    LocatedToken DATABASE _ -> Just ()
    _ -> Nothing

default_ :: Terminal r ()
default_ =
  Earley.terminal \case
    LocatedToken DEFAULT _ -> Just ()
    _ -> Nothing

deferrable :: Terminal r ()
deferrable =
  Earley.terminal \case
    LocatedToken DEFERRABLE _ -> Just ()
    _ -> Nothing

deferred :: Terminal r Token
deferred =
  Earley.terminal \case
    LocatedToken DEFERRED _ -> Just DEFERRED
    _ -> Nothing

delete :: Terminal r ()
delete =
  Earley.terminal \case
    LocatedToken DELETE _ -> Just ()
    _ -> Nothing

desc :: Terminal r ()
desc =
  Earley.terminal \case
    LocatedToken DESC _ -> Just ()
    _ -> Nothing

distinct :: Terminal r ()
distinct =
  Earley.terminal \case
    LocatedToken DISTINCT _ -> Just ()
    _ -> Nothing

drop :: Terminal r ()
drop =
  Earley.terminal \case
    LocatedToken DROP _ -> Just ()
    _ -> Nothing

else_ :: Terminal r ()
else_ =
  Earley.terminal \case
    LocatedToken ELSE _ -> Just ()
    _ -> Nothing

end :: Terminal r ()
end =
  Earley.terminal \case
    LocatedToken END _ -> Just ()
    _ -> Nothing

equalsSign :: Terminal r ()
equalsSign =
  Earley.terminal \case
    LocatedToken EqualsSign _ -> Just ()
    _ -> Nothing

equalsSignEqualsSign :: Terminal r ()
equalsSignEqualsSign =
  Earley.terminal \case
    LocatedToken EqualsSignEqualsSign _ -> Just ()
    _ -> Nothing

escape :: Terminal r ()
escape =
  Earley.terminal \case
    LocatedToken ESCAPE _ -> Just ()
    _ -> Nothing

except :: Terminal r ()
except =
  Earley.terminal \case
    LocatedToken EXCEPT _ -> Just ()
    _ -> Nothing

exclamationMarkEqualsSign :: Terminal r ()
exclamationMarkEqualsSign =
  Earley.terminal \case
    LocatedToken ExclamationMarkEqualsSign _ -> Just ()
    _ -> Nothing

exclude :: Terminal r ()
exclude =
  Earley.terminal \case
    LocatedToken EXCLUDE _ -> Just ()
    _ -> Nothing

exclusive :: Terminal r ()
exclusive =
  Earley.terminal \case
    LocatedToken EXCLUSIVE _ -> Just ()
    _ -> Nothing

exists :: Terminal r ()
exists =
  Earley.terminal \case
    LocatedToken EXISTS _ -> Just ()
    _ -> Nothing

fail :: Terminal r ()
fail =
  Earley.terminal \case
    LocatedToken FAIL _ -> Just ()
    _ -> Nothing

false :: Terminal r ()
false =
  Earley.terminal \case
    LocatedToken FALSE _ -> Just ()
    _ -> Nothing

filter :: Terminal r ()
filter =
  Earley.terminal \case
    LocatedToken FILTER _ -> Just ()
    _ -> Nothing

first :: Terminal r ()
first =
  Earley.terminal \case
    LocatedToken FIRST _ -> Just ()
    _ -> Nothing

following :: Terminal r ()
following =
  Earley.terminal \case
    LocatedToken FOLLOWING _ -> Just ()
    _ -> Nothing

foreign_ :: Terminal r ()
foreign_ =
  Earley.terminal \case
    LocatedToken FOREIGN _ -> Just ()
    _ -> Nothing

from :: Terminal r ()
from =
  Earley.terminal \case
    LocatedToken FROM _ -> Just ()
    _ -> Nothing

fullStop :: Terminal r ()
fullStop =
  Earley.terminal \case
    LocatedToken FullStop _ -> Just ()
    _ -> Nothing

generated :: Terminal r ()
generated =
  Earley.terminal \case
    LocatedToken GENERATED _ -> Just ()
    _ -> Nothing

glob :: Terminal r ()
glob =
  Earley.terminal \case
    LocatedToken GLOB _ -> Just ()
    _ -> Nothing

greaterThanSign :: Terminal r ()
greaterThanSign =
  Earley.terminal \case
    LocatedToken GreaterThanSign _ -> Just ()
    _ -> Nothing

greaterThanSignEqualsSign :: Terminal r ()
greaterThanSignEqualsSign =
  Earley.terminal \case
    LocatedToken GreaterThanSignEqualsSign _ -> Just ()
    _ -> Nothing

greaterThanSignGreaterThanSign :: Terminal r ()
greaterThanSignGreaterThanSign =
  Earley.terminal \case
    LocatedToken GreaterThanSignGreaterThanSign _ -> Just ()
    _ -> Nothing

group :: Terminal r ()
group =
  Earley.terminal \case
    LocatedToken GROUP _ -> Just ()
    _ -> Nothing

groups :: Terminal r ()
groups =
  Earley.terminal \case
    LocatedToken GROUPS _ -> Just ()
    _ -> Nothing

having :: Terminal r ()
having =
  Earley.terminal \case
    LocatedToken HAVING _ -> Just ()
    _ -> Nothing

hyphenMinus :: Terminal r ()
hyphenMinus =
  Earley.terminal \case
    LocatedToken HyphenMinus _ -> Just ()
    _ -> Nothing

identifier :: Terminal r Text
identifier =
  Earley.terminal \case
    LocatedToken (Identifier s) _ -> Just s
    _ -> Nothing

if_ :: Terminal r ()
if_ =
  Earley.terminal \case
    LocatedToken IF _ -> Just ()
    _ -> Nothing

ignore :: Terminal r ()
ignore =
  Earley.terminal \case
    LocatedToken IGNORE _ -> Just ()
    _ -> Nothing

immediate :: Terminal r Token
immediate =
  Earley.terminal \case
    LocatedToken IMMEDIATE _ -> Just IMMEDIATE
    _ -> Nothing

in_ :: Terminal r ()
in_ =
  Earley.terminal \case
    LocatedToken IN _ -> Just ()
    _ -> Nothing

index :: Terminal r ()
index =
  Earley.terminal \case
    LocatedToken INDEX _ -> Just ()
    _ -> Nothing

indexed :: Terminal r ()
indexed =
  Earley.terminal \case
    LocatedToken INDEXED _ -> Just ()
    _ -> Nothing

initially :: Terminal r ()
initially =
  Earley.terminal \case
    LocatedToken INITIALLY _ -> Just ()
    _ -> Nothing

inner :: Terminal r ()
inner =
  Earley.terminal \case
    LocatedToken INNER _ -> Just ()
    _ -> Nothing

intersect :: Terminal r ()
intersect =
  Earley.terminal \case
    LocatedToken INTERSECT _ -> Just ()
    _ -> Nothing

is :: Terminal r ()
is =
  Earley.terminal \case
    LocatedToken IS _ -> Just ()
    _ -> Nothing

isnull :: Terminal r ()
isnull =
  Earley.terminal \case
    LocatedToken ISNULL _ -> Just ()
    _ -> Nothing

join :: Terminal r ()
join =
  Earley.terminal \case
    LocatedToken JOIN _ -> Just ()
    _ -> Nothing

key :: Terminal r ()
key =
  Earley.terminal \case
    LocatedToken KEY _ -> Just ()
    _ -> Nothing

last :: Terminal r ()
last =
  Earley.terminal \case
    LocatedToken LAST _ -> Just ()
    _ -> Nothing

left :: Terminal r ()
left =
  Earley.terminal \case
    LocatedToken LEFT _ -> Just ()
    _ -> Nothing

leftParenthesis :: Terminal r ()
leftParenthesis =
  Earley.terminal \case
    LocatedToken LeftParenthesis _ -> Just ()
    _ -> Nothing

lessThanSign :: Terminal r ()
lessThanSign =
  Earley.terminal \case
    LocatedToken LessThanSign _ -> Just ()
    _ -> Nothing

lessThanSignEqualsSign :: Terminal r ()
lessThanSignEqualsSign =
  Earley.terminal \case
    LocatedToken LessThanSignEqualsSign _ -> Just ()
    _ -> Nothing

lessThanSignGreaterThanSign :: Terminal r ()
lessThanSignGreaterThanSign =
  Earley.terminal \case
    LocatedToken LessThanSignGreaterThanSign _ -> Just ()
    _ -> Nothing

lessThanSignLessThanSign :: Terminal r ()
lessThanSignLessThanSign =
  Earley.terminal \case
    LocatedToken LessThanSignLessThanSign _ -> Just ()
    _ -> Nothing

like :: Terminal r ()
like =
  Earley.terminal \case
    LocatedToken LIKE _ -> Just ()
    _ -> Nothing

limit :: Terminal r ()
limit =
  Earley.terminal \case
    LocatedToken LIMIT _ -> Just ()
    _ -> Nothing

match :: Terminal r ()
match =
  Earley.terminal \case
    LocatedToken MATCH _ -> Just ()
    _ -> Nothing

materialized :: Terminal r ()
materialized =
  Earley.terminal \case
    LocatedToken MATERIALIZED _ -> Just ()
    _ -> Nothing

namedParameter :: Terminal r Text
namedParameter =
  Earley.terminal \case
    LocatedToken (NamedParameter s) _ -> Just s
    _ -> Nothing

natural :: Terminal r ()
natural =
  Earley.terminal \case
    LocatedToken NATURAL _ -> Just ()
    _ -> Nothing

no :: Terminal r ()
no =
  Earley.terminal \case
    LocatedToken NO _ -> Just ()
    _ -> Nothing

not :: Terminal r ()
not =
  Earley.terminal \case
    LocatedToken NOT _ -> Just ()
    _ -> Nothing

notnull :: Terminal r ()
notnull =
  Earley.terminal \case
    LocatedToken NOTNULL _ -> Just ()
    _ -> Nothing

null :: Terminal r ()
null =
  Earley.terminal \case
    LocatedToken NULL _ -> Just ()
    _ -> Nothing

nulls :: Terminal r ()
nulls =
  Earley.terminal \case
    LocatedToken NULLS _ -> Just ()
    _ -> Nothing

number :: Terminal r Text
number =
  Earley.terminal \case
    LocatedToken (Number s) _ -> Just s
    _ -> Nothing

offset :: Terminal r ()
offset =
  Earley.terminal \case
    LocatedToken OFFSET _ -> Just ()
    _ -> Nothing

on :: Terminal r ()
on =
  Earley.terminal \case
    LocatedToken ON _ -> Just ()
    _ -> Nothing

or :: Terminal r ()
or =
  Earley.terminal \case
    LocatedToken OR _ -> Just ()
    _ -> Nothing

order :: Terminal r ()
order =
  Earley.terminal \case
    LocatedToken ORDER _ -> Just ()
    _ -> Nothing

others :: Terminal r ()
others =
  Earley.terminal \case
    LocatedToken OTHERS _ -> Just ()
    _ -> Nothing

outer :: Terminal r ()
outer =
  Earley.terminal \case
    LocatedToken OUTER _ -> Just ()
    _ -> Nothing

over :: Terminal r ()
over =
  Earley.terminal \case
    LocatedToken OVER _ -> Just ()
    _ -> Nothing

parameter :: Terminal r (Maybe Natural)
parameter =
  Earley.terminal \case
    LocatedToken (Parameter n) _ -> Just n
    _ -> Nothing

partition :: Terminal r ()
partition =
  Earley.terminal \case
    LocatedToken PARTITION _ -> Just ()
    _ -> Nothing

percentSign :: Terminal r ()
percentSign =
  Earley.terminal \case
    LocatedToken PercentSign _ -> Just ()
    _ -> Nothing

plusSign :: Terminal r ()
plusSign =
  Earley.terminal \case
    LocatedToken PlusSign _ -> Just ()
    _ -> Nothing

preceding :: Terminal r ()
preceding =
  Earley.terminal \case
    LocatedToken PRECEDING _ -> Just ()
    _ -> Nothing

primary :: Terminal r ()
primary =
  Earley.terminal \case
    LocatedToken PRIMARY _ -> Just ()
    _ -> Nothing

raise :: Terminal r ()
raise =
  Earley.terminal \case
    LocatedToken RAISE _ -> Just ()
    _ -> Nothing

range :: Terminal r ()
range =
  Earley.terminal \case
    LocatedToken RANGE _ -> Just ()
    _ -> Nothing

recursive :: Terminal r ()
recursive =
  Earley.terminal \case
    LocatedToken RECURSIVE _ -> Just ()
    _ -> Nothing

references :: Terminal r ()
references =
  Earley.terminal \case
    LocatedToken REFERENCES _ -> Just ()
    _ -> Nothing

regexp :: Terminal r ()
regexp =
  Earley.terminal \case
    LocatedToken REGEXP _ -> Just ()
    _ -> Nothing

rename :: Terminal r ()
rename =
  Earley.terminal \case
    LocatedToken RENAME _ -> Just ()
    _ -> Nothing

replace :: Terminal r ()
replace =
  Earley.terminal \case
    LocatedToken REPLACE _ -> Just ()
    _ -> Nothing

restrict :: Terminal r ()
restrict =
  Earley.terminal \case
    LocatedToken RESTRICT _ -> Just ()
    _ -> Nothing

rightParenthesis :: Terminal r ()
rightParenthesis =
  Earley.terminal \case
    LocatedToken RightParenthesis _ -> Just ()
    _ -> Nothing

rollback :: Terminal r ()
rollback =
  Earley.terminal \case
    LocatedToken ROLLBACK _ -> Just ()
    _ -> Nothing

row :: Terminal r ()
row =
  Earley.terminal \case
    LocatedToken ROW _ -> Just ()
    _ -> Nothing

rowid :: Terminal r ()
rowid = do
  Earley.terminal \case
    LocatedToken (Identifier s) _ | Text.toCaseFold s == "rowid" -> Just ()
    _ -> Nothing

rows :: Terminal r ()
rows =
  Earley.terminal \case
    LocatedToken ROWS _ -> Just ()
    _ -> Nothing

savepoint :: Terminal r ()
savepoint =
  Earley.terminal \case
    LocatedToken SAVEPOINT _ -> Just ()
    _ -> Nothing

select :: Terminal r ()
select =
  Earley.terminal \case
    LocatedToken SELECT _ -> Just ()
    _ -> Nothing

set :: Terminal r ()
set =
  Earley.terminal \case
    LocatedToken SET _ -> Just ()
    _ -> Nothing

solidus :: Terminal r ()
solidus =
  Earley.terminal \case
    LocatedToken Solidus _ -> Just ()
    _ -> Nothing

stored :: Terminal r ()
stored =
  Earley.terminal \case
    LocatedToken STORED _ -> Just ()
    _ -> Nothing

string :: Terminal r Text
string =
  Earley.terminal \case
    LocatedToken (String s) _ -> Just s
    _ -> Nothing

table :: Terminal r ()
table =
  Earley.terminal \case
    LocatedToken TABLE _ -> Just ()
    _ -> Nothing

then_ :: Terminal r ()
then_ =
  Earley.terminal \case
    LocatedToken THEN _ -> Just ()
    _ -> Nothing

ties :: Terminal r ()
ties =
  Earley.terminal \case
    LocatedToken TIES _ -> Just ()
    _ -> Nothing

to :: Terminal r ()
to =
  Earley.terminal \case
    LocatedToken TO _ -> Just ()
    _ -> Nothing

temp :: Terminal r ()
temp =
  Earley.terminal \case
    LocatedToken TEMP _ -> Just ()
    _ -> Nothing

temporary :: Terminal r ()
temporary =
  Earley.terminal \case
    LocatedToken TEMPORARY _ -> Just ()
    _ -> Nothing

tilde :: Terminal r ()
tilde =
  Earley.terminal \case
    LocatedToken Tilde _ -> Just ()
    _ -> Nothing

transaction :: Terminal r ()
transaction =
  Earley.terminal \case
    LocatedToken TRANSACTION _ -> Just ()
    _ -> Nothing

true :: Terminal r ()
true =
  Earley.terminal \case
    LocatedToken TRUE _ -> Just ()
    _ -> Nothing

unbounded :: Terminal r ()
unbounded =
  Earley.terminal \case
    LocatedToken UNBOUNDED _ -> Just ()
    _ -> Nothing

union :: Terminal r ()
union =
  Earley.terminal \case
    LocatedToken UNION _ -> Just ()
    _ -> Nothing

unique :: Terminal r ()
unique =
  Earley.terminal \case
    LocatedToken UNIQUE _ -> Just ()
    _ -> Nothing

update :: Terminal r ()
update =
  Earley.terminal \case
    LocatedToken UPDATE _ -> Just ()
    _ -> Nothing

using :: Terminal r ()
using =
  Earley.terminal \case
    LocatedToken USING _ -> Just ()
    _ -> Nothing

values :: Terminal r ()
values =
  Earley.terminal \case
    LocatedToken VALUES _ -> Just ()
    _ -> Nothing

verticalLine :: Terminal r ()
verticalLine =
  Earley.terminal \case
    LocatedToken VerticalLine _ -> Just ()
    _ -> Nothing

verticalLineVerticalLine :: Terminal r ()
verticalLineVerticalLine =
  Earley.terminal \case
    LocatedToken VerticalLineVerticalLine _ -> Just ()
    _ -> Nothing

virtual :: Terminal r ()
virtual =
  Earley.terminal \case
    LocatedToken VIRTUAL _ -> Just ()
    _ -> Nothing

when :: Terminal r ()
when =
  Earley.terminal \case
    LocatedToken WHEN _ -> Just ()
    _ -> Nothing

where_ :: Terminal r ()
where_ =
  Earley.terminal \case
    LocatedToken WHERE _ -> Just ()
    _ -> Nothing

window :: Terminal r ()
window =
  Earley.terminal \case
    LocatedToken WINDOW _ -> Just ()
    _ -> Nothing

with :: Terminal r ()
with =
  Earley.terminal \case
    LocatedToken WITH _ -> Just ()
    _ -> Nothing

without :: Terminal r ()
without =
  Earley.terminal \case
    LocatedToken WITHOUT _ -> Just ()
    _ -> Nothing
