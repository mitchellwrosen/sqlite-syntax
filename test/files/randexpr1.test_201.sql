-- randexpr1.test
-- 
-- db eval {SELECT case when t1.e*t1.b<>e then 19 else  -11-b+coalesce((select max(t1.e+t1.f) from t1 where 11+f*t1.c<>t1.a*(~f+case when t1.f in (t1.f, -((13)),19) then t1.a when t1.e>17 then a else a end+b-t1.e-(f))),13)+d end+t1.b* -c FROM t1 WHERE NOT (exists(select 1 from t1 where coalesce((select max(t1.a*t1.e+13) from t1 where 13 in ((abs(e)/abs(t1.a)),a+t1.c,17*t1.c)),(select  -case max(c) | cast(avg(e) AS integer) when ~+ -count(distinct coalesce((select d from t1 where exists(select 1 from t1 where  -17<e)),( -t1.e))) | case max(11) when cast(avg(a) AS integer) then max( -t1.f) else count(distinct  -11) end then cast(avg(t1.d) AS integer) else max((d)) end*count(distinct t1.b) from t1) | t1.b-t1.d*t1.c+11)<=b))}
SELECT case when t1.e*t1.b<>e then 19 else  -11-b+coalesce((select max(t1.e+t1.f) from t1 where 11+f*t1.c<>t1.a*(~f+case when t1.f in (t1.f, -((13)),19) then t1.a when t1.e>17 then a else a end+b-t1.e-(f))),13)+d end+t1.b* -c FROM t1 WHERE NOT (exists(select 1 from t1 where coalesce((select max(t1.a*t1.e+13) from t1 where 13 in ((abs(e)/abs(t1.a)),a+t1.c,17*t1.c)),(select  -case max(c) | cast(avg(e) AS integer) when ~+ -count(distinct coalesce((select d from t1 where exists(select 1 from t1 where  -17<e)),( -t1.e))) | case max(11) when cast(avg(a) AS integer) then max( -t1.f) else count(distinct  -11) end then cast(avg(t1.d) AS integer) else max((d)) end*count(distinct t1.b) from t1) | t1.b-t1.d*t1.c+11)<=b))