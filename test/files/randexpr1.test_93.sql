-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select +19 from t1 where 19-c+t1.c-f not in (coalesce((select 19 from t1 where (t1.b between t1.c*t1.a*t1.b | a-e | 13-(abs(19)/abs((select max(t1.d | coalesce((select max( -t1.c) from t1 where b in (select e from t1 union select (f) from t1)),t1.d)+(e)) from t1)+c)) and f)),11),f,17)),f) FROM t1 WHERE (select count(distinct 11)+min((coalesce((select max(case when t1.d<>(abs(t1.a)/abs(e)) then d else ~(t1.e)-e+d+13-t1.b+b*19 end) from t1 where (select max(19)-cast(avg(a) AS integer) from t1)> -t1.e),17)+e+t1.e-t1.c+(c))*17) from t1) between  -17 and t1.c}
SELECT coalesce((select +19 from t1 where 19-c+t1.c-f not in (coalesce((select 19 from t1 where (t1.b between t1.c*t1.a*t1.b | a-e | 13-(abs(19)/abs((select max(t1.d | coalesce((select max( -t1.c) from t1 where b in (select e from t1 union select (f) from t1)),t1.d)+(e)) from t1)+c)) and f)),11),f,17)),f) FROM t1 WHERE (select count(distinct 11)+min((coalesce((select max(case when t1.d<>(abs(t1.a)/abs(e)) then d else ~(t1.e)-e+d+13-t1.b+b*19 end) from t1 where (select max(19)-cast(avg(a) AS integer) from t1)> -t1.e),17)+e+t1.e-t1.c+(c))*17) from t1) between  -17 and t1.c