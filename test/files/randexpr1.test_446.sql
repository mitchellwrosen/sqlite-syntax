-- randexpr1.test
-- 
-- db eval {SELECT e+c-(c)+t1.e*(abs(case when 13 in (select abs(max(17)*case  -count(*) when cast(avg(a) AS integer) then count(distinct b) else  -max(f) end | (cast(avg(t1.e) AS integer)) |  -min(f)) from t1 union select count(distinct d) from t1) and case when (t1.e+a> -(a)) then e when not c<t1.f or t1.d=f then e else t1.e end<=t1.f then  -a when t1.d<=13 then a else t1.b end)/abs( -t1.a))-t1.b+t1.b FROM t1 WHERE case when  -case when t1.f<t1.c then  -(t1.e) when 11 between e and coalesce((select c from t1 where not exists(select 1 from t1 where b>=e)),coalesce((select max(case t1.e when t1.c then t1.a else a end) from t1 where b>=t1.c and b<>t1.c),13)) then t1.b else a end<11 then b when (11 in (select a from t1 union select t1.b from t1)) or t1.b=e or a between a and 13 then t1.b else b end>19}
SELECT e+c-(c)+t1.e*(abs(case when 13 in (select abs(max(17)*case  -count(*) when cast(avg(a) AS integer) then count(distinct b) else  -max(f) end | (cast(avg(t1.e) AS integer)) |  -min(f)) from t1 union select count(distinct d) from t1) and case when (t1.e+a> -(a)) then e when not c<t1.f or t1.d=f then e else t1.e end<=t1.f then  -a when t1.d<=13 then a else t1.b end)/abs( -t1.a))-t1.b+t1.b FROM t1 WHERE case when  -case when t1.f<t1.c then  -(t1.e) when 11 between e and coalesce((select c from t1 where not exists(select 1 from t1 where b>=e)),coalesce((select max(case t1.e when t1.c then t1.a else a end) from t1 where b>=t1.c and b<>t1.c),13)) then t1.b else a end<11 then b when (11 in (select a from t1 union select t1.b from t1)) or t1.b=e or a between a and 13 then t1.b else b end>19