-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select e from t1 where (coalesce((select max(+e+e*11*t1.c-e) from t1 where exists(select 1 from t1 where (19<=~t1.f))),case (abs(t1.c)/abs(case t1.c when 19 then f*~case when t1.e+t1.d between  -t1.f and (d) then b when (d=a) then t1.e else e end else c end*t1.a)) & f when (d) then 17 else t1.f end)<t1.c)),13) FROM t1 WHERE NOT (a>coalesce((select coalesce((select e from t1 where case when  -d+t1.e>e then coalesce((select d+b from t1 where exists(select 1 from t1 where not 13-b not in (f,a,d)) and (t1.b between (a) and t1.c)),c) when a=13 then t1.e else a end in (select t1.d from t1 union select t1.d from t1) and e not in (d, -19,11)),d) from t1 where not f in (t1.a,19,t1.d)),t1.d))}
SELECT coalesce((select e from t1 where (coalesce((select max(+e+e*11*t1.c-e) from t1 where exists(select 1 from t1 where (19<=~t1.f))),case (abs(t1.c)/abs(case t1.c when 19 then f*~case when t1.e+t1.d between  -t1.f and (d) then b when (d=a) then t1.e else e end else c end*t1.a)) & f when (d) then 17 else t1.f end)<t1.c)),13) FROM t1 WHERE NOT (a>coalesce((select coalesce((select e from t1 where case when  -d+t1.e>e then coalesce((select d+b from t1 where exists(select 1 from t1 where not 13-b not in (f,a,d)) and (t1.b between (a) and t1.c)),c) when a=13 then t1.e else a end in (select t1.d from t1 union select t1.d from t1) and e not in (d, -19,11)),d) from t1 where not f in (t1.a,19,t1.d)),t1.d))