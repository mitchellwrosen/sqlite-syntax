-- randexpr1.test
-- 
-- db eval {SELECT case when coalesce((select +case when not exists(select 1 from t1 where 19*t1.e< -~11*a or t1.b>=t1.c and t1.a<>e) then coalesce((select max(13) from t1 where 11>11), -(e)) | t1.a when (t1.d) in (select f from t1 union select b from t1) then a else e end from t1 where t1.f<> -t1.d),t1.b) not in (d,t1.d,t1.c) then 19 when (t1.b<>t1.e or  -t1.b>=t1.b) then (f) else d end FROM t1 WHERE t1.e=a and b>=(abs(case when 19 not between ~+17 and +case when ((b+e-f=t1.d or b not in (t1.a,t1.f,t1.b) or t1.a not between 19 and e or b>=(t1.d) or t1.b not in (d,t1.a,t1.b))) then e-f else t1.e*t1.f end | f then d else  -d end)/abs(11))}
SELECT case when coalesce((select +case when not exists(select 1 from t1 where 19*t1.e< -~11*a or t1.b>=t1.c and t1.a<>e) then coalesce((select max(13) from t1 where 11>11), -(e)) | t1.a when (t1.d) in (select f from t1 union select b from t1) then a else e end from t1 where t1.f<> -t1.d),t1.b) not in (d,t1.d,t1.c) then 19 when (t1.b<>t1.e or  -t1.b>=t1.b) then (f) else d end FROM t1 WHERE t1.e=a and b>=(abs(case when 19 not between ~+17 and +case when ((b+e-f=t1.d or b not in (t1.a,t1.f,t1.b) or t1.a not between 19 and e or b>=(t1.d) or t1.b not in (d,t1.a,t1.b))) then e-f else t1.e*t1.f end | f then d else  -d end)/abs(11))