-- randexpr1.test
-- 
-- db eval {SELECT a-coalesce((select max(++19) from t1 where (select count(distinct coalesce((select max(t1.c) from t1 where not (t1.c+t1.c)-case when (not coalesce((select max(t1.a-t1.a) from t1 where t1.d>t1.b),e) in (t1.d,19,(t1.e))) then b when exists(select 1 from t1 where t1.e> -13) then c else e end<>c),17)) from t1) not between 19 and t1.f),t1.b) | 19-t1.d*f+b FROM t1 WHERE NOT (t1.f | coalesce((select case when c>=t1.f then t1.e+t1.e when  -19 | (a)+t1.f in (b,coalesce((select t1.c | (f+e)-e*17 from t1 where  -c not in (t1.d,c,(13))),19),t1.e) then t1.c else 17 end+b from t1 where t1.a not in (17,d,c)),17) in (select f from t1 union select f from t1))}
SELECT a-coalesce((select max(++19) from t1 where (select count(distinct coalesce((select max(t1.c) from t1 where not (t1.c+t1.c)-case when (not coalesce((select max(t1.a-t1.a) from t1 where t1.d>t1.b),e) in (t1.d,19,(t1.e))) then b when exists(select 1 from t1 where t1.e> -13) then c else e end<>c),17)) from t1) not between 19 and t1.f),t1.b) | 19-t1.d*f+b FROM t1 WHERE NOT (t1.f | coalesce((select case when c>=t1.f then t1.e+t1.e when  -19 | (a)+t1.f in (b,coalesce((select t1.c | (f+e)-e*17 from t1 where  -c not in (t1.d,c,(13))),19),t1.e) then t1.c else 17 end+b from t1 where t1.a not in (17,d,c)),17) in (select f from t1 union select f from t1))