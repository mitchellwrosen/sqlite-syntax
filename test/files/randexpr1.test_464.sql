-- randexpr1.test
-- 
-- db eval {SELECT (case case when +c>e then case t1.d when 13 then t1.c else 19 end-13 else t1.e+coalesce((select max(19) from t1 where case when e in (+b,coalesce((select a+(11) from t1 where b not between c and d or t1.b not in (a,11,17)),b),11) then 19 when (c) not in (t1.c,t1.b,d) then t1.c else t1.d end*f not between t1.a and d),13) end when c then 11 else c end) FROM t1 WHERE NOT (19 not between coalesce((select max(d*t1.c) from t1 where (select cast(avg((b)) AS integer) from t1) in (select ( - -(abs(case max(b) when cast(avg( -13-11) AS integer) then count(distinct a-e) else ~ -cast(avg((c)) AS integer)-min(17)+count(*) end) | (min(e)))-min((19))) from t1 union select count(distinct 13) from t1)),case when  -e | 19 between  -11 and f then 19 else 19 end) and (f))}
SELECT (case case when +c>e then case t1.d when 13 then t1.c else 19 end-13 else t1.e+coalesce((select max(19) from t1 where case when e in (+b,coalesce((select a+(11) from t1 where b not between c and d or t1.b not in (a,11,17)),b),11) then 19 when (c) not in (t1.c,t1.b,d) then t1.c else t1.d end*f not between t1.a and d),13) end when c then 11 else c end) FROM t1 WHERE NOT (19 not between coalesce((select max(d*t1.c) from t1 where (select cast(avg((b)) AS integer) from t1) in (select ( - -(abs(case max(b) when cast(avg( -13-11) AS integer) then count(distinct a-e) else ~ -cast(avg((c)) AS integer)-min(17)+count(*) end) | (min(e)))-min((19))) from t1 union select count(distinct 13) from t1)),case when  -e | 19 between  -11 and f then 19 else 19 end) and (f))