-- randexpr1.test
-- 
-- db eval {SELECT (abs(coalesce((select max(a) from t1 where exists(select 1 from t1 where t1.f<19 and coalesce((select max(case when t1.a+e in (select t1.c from t1 union select f from t1) then f else (17) end) from t1 where t1.e in (f,coalesce((select max(t1.f) from t1 where 17<=((abs(t1.d & (t1.a))/abs(13))*(a))-d),17),e)),t1.c) not between t1.b and 11 and 13<t1.f)),f+t1.d))/abs(19)) FROM t1 WHERE NOT (11*c+17-f in (select min(t1.e*11*case when exists(select 1 from t1 where not exists(select 1 from t1 where + -17<a)) then (abs((d))/abs(13)) when 11 in (select case count(*) when count(distinct t1.f) then count(distinct 17) else cast(avg((11)) AS integer) end from t1 union select cast(avg(t1.c) AS integer) from t1) then b else e end) from t1 union select case count(*) when  -count(distinct t1.d) then max(a) else (count(distinct t1.c))-max((b)) | (count(distinct t1.e))*count(distinct (13))-count(distinct e) end-count(*)-count(*) from t1))}
SELECT (abs(coalesce((select max(a) from t1 where exists(select 1 from t1 where t1.f<19 and coalesce((select max(case when t1.a+e in (select t1.c from t1 union select f from t1) then f else (17) end) from t1 where t1.e in (f,coalesce((select max(t1.f) from t1 where 17<=((abs(t1.d & (t1.a))/abs(13))*(a))-d),17),e)),t1.c) not between t1.b and 11 and 13<t1.f)),f+t1.d))/abs(19)) FROM t1 WHERE NOT (11*c+17-f in (select min(t1.e*11*case when exists(select 1 from t1 where not exists(select 1 from t1 where + -17<a)) then (abs((d))/abs(13)) when 11 in (select case count(*) when count(distinct t1.f) then count(distinct 17) else cast(avg((11)) AS integer) end from t1 union select cast(avg(t1.c) AS integer) from t1) then b else e end) from t1 union select case count(*) when  -count(distinct t1.d) then max(a) else (count(distinct t1.c))-max((b)) | (count(distinct t1.e))*count(distinct (13))-count(distinct e) end-count(*)-count(*) from t1))