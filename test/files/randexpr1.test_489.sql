-- randexpr1.test
-- 
-- db eval {SELECT case  -coalesce((select max(case when case when t1.e in (11*t1.e*17,(select (min(b) |  - -+cast(avg(17) AS integer)*max((t1.c))+cast(avg(t1.a) AS integer))+(( -(min(t1.c))))*count(*) from t1), -~b*t1.d+t1.d) then d else t1.a end<(t1.c) then t1.f else e end) from t1 where 13<>c),e)-(t1.b) when t1.a then 17 else t1.f end FROM t1 WHERE coalesce((select max(t1.e) from t1 where (not exists(select 1 from t1 where b<+c))),case when b | ~case t1.a when  -t1.b then  -t1.f else t1.f end*f+f in (select t1.b from t1 union select t1.c from t1) then  -(t1.f) when a<>11 then (d) else 11 end) in (select abs(~(abs(case (min(t1.b)*abs(count(distinct a)+count(*) |  -count(distinct a)))- -max(19) when max(t1.e) then min(d) else (max(a)) end))) from t1 union select count(*) from t1)}
SELECT case  -coalesce((select max(case when case when t1.e in (11*t1.e*17,(select (min(b) |  - -+cast(avg(17) AS integer)*max((t1.c))+cast(avg(t1.a) AS integer))+(( -(min(t1.c))))*count(*) from t1), -~b*t1.d+t1.d) then d else t1.a end<(t1.c) then t1.f else e end) from t1 where 13<>c),e)-(t1.b) when t1.a then 17 else t1.f end FROM t1 WHERE coalesce((select max(t1.e) from t1 where (not exists(select 1 from t1 where b<+c))),case when b | ~case t1.a when  -t1.b then  -t1.f else t1.f end*f+f in (select t1.b from t1 union select t1.c from t1) then  -(t1.f) when a<>11 then (d) else 11 end) in (select abs(~(abs(case (min(t1.b)*abs(count(distinct a)+count(*) |  -count(distinct a)))- -max(19) when max(t1.e) then min(d) else (max(a)) end))) from t1 union select count(*) from t1)