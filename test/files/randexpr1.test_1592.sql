-- randexpr1.test
-- 
-- db eval {SELECT c | case when e<=(abs(case when case when not exists(select 1 from t1 where case when t1.f>case when 19>=17 or f in (19,c,t1.d) then t1.b else b end then t1.e else  -t1.b end*t1.d in (select t1.e from t1 union select c from t1)) then e else f end<>t1.d then  -a when (11 in (select b from t1 union select a from t1)) then 19 else t1.a end-(c))/abs(c))+t1.d | t1.a then  -19 else 17 end+t1.d-(t1.a) FROM t1 WHERE NOT (coalesce((select 13 from t1 where not exists(select 1 from t1 where t1.e*t1.d*t1.f>=(11) and d in (13,coalesce((select t1.f+t1.b from t1 where coalesce((select max(t1.a) from t1 where not (t1.d)<=t1.c),c) not in (t1.e,t1.d,b)),b)+f,d)) or (t1.f>f) or 11<=t1.d and 17<>13),a)<(t1.a))}
SELECT c | case when e<=(abs(case when case when not exists(select 1 from t1 where case when t1.f>case when 19>=17 or f in (19,c,t1.d) then t1.b else b end then t1.e else  -t1.b end*t1.d in (select t1.e from t1 union select c from t1)) then e else f end<>t1.d then  -a when (11 in (select b from t1 union select a from t1)) then 19 else t1.a end-(c))/abs(c))+t1.d | t1.a then  -19 else 17 end+t1.d-(t1.a) FROM t1 WHERE NOT (coalesce((select 13 from t1 where not exists(select 1 from t1 where t1.e*t1.d*t1.f>=(11) and d in (13,coalesce((select t1.f+t1.b from t1 where coalesce((select max(t1.a) from t1 where not (t1.d)<=t1.c),c) not in (t1.e,t1.d,b)),b)+f,d)) or (t1.f>f) or 11<=t1.d and 17<>13),a)<(t1.a))