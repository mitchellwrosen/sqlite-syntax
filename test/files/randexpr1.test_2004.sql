-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select 11 from t1 where a not in (17,e,(select  -abs(case ( -count(distinct t1.e)) when count(distinct 13) & max((abs(case 11 when  -(abs(19)/abs(17 & t1.c*t1.e))*t1.c & t1.b then a else +d end)/abs(b*t1.a))) then max(c*13) else cast(avg(t1.f) AS integer) end)-max(11) from t1)*d)),t1.d) FROM t1 WHERE NOT (19-c*coalesce((select max(case (select count(distinct t1.f) from t1) | t1.a when case a when (abs(case when not c+t1.d>=e then 17*t1.e+a when not exists(select 1 from t1 where 11>b) then c else a end)/abs(t1.e))+t1.f*13 then 11 else t1.f end then 19 else d end-b) from t1 where (d) not between f and e),t1.d)= -c)}
SELECT coalesce((select 11 from t1 where a not in (17,e,(select  -abs(case ( -count(distinct t1.e)) when count(distinct 13) & max((abs(case 11 when  -(abs(19)/abs(17 & t1.c*t1.e))*t1.c & t1.b then a else +d end)/abs(b*t1.a))) then max(c*13) else cast(avg(t1.f) AS integer) end)-max(11) from t1)*d)),t1.d) FROM t1 WHERE NOT (19-c*coalesce((select max(case (select count(distinct t1.f) from t1) | t1.a when case a when (abs(case when not c+t1.d>=e then 17*t1.e+a when not exists(select 1 from t1 where 11>b) then c else a end)/abs(t1.e))+t1.f*13 then 11 else t1.f end then 19 else d end-b) from t1 where (d) not between f and e),t1.d)= -c)