-- randexpr1.test
-- 
-- db eval {SELECT t1.d+case when (t1.a)<=11 then ~t1.a else ~19 end-+~ -coalesce((select t1.d from t1 where ~t1.f in (select (cast(avg(t1.e+c) AS integer)) from t1 union select count(distinct (abs(coalesce((select max(19) from t1 where not ~11-t1.a=t1.c+case when t1.d between c and t1.d then 11 when t1.f>=17 then f else 19 end*t1.c),17)+f)/abs(f))) from t1)),11) FROM t1 WHERE e>c}
SELECT t1.d+case when (t1.a)<=11 then ~t1.a else ~19 end-+~ -coalesce((select t1.d from t1 where ~t1.f in (select (cast(avg(t1.e+c) AS integer)) from t1 union select count(distinct (abs(coalesce((select max(19) from t1 where not ~11-t1.a=t1.c+case when t1.d between c and t1.d then 11 when t1.f>=17 then f else 19 end*t1.c),17)+f)/abs(f))) from t1)),11) FROM t1 WHERE e>c