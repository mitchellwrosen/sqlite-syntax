-- randexpr1.test
-- 
-- db eval {SELECT ~case when t1.e<=t1.a then b else (11) end+(select  -((cast(avg(coalesce((select a from t1 where (select cast(avg(t1.d) AS integer) | case case min((t1.e)) when cast(avg(t1.b) AS integer) then max(t1.c) else max(19) end*count(*) when  -count(distinct e) then count(*) else max( - -17) end from t1) not between c and  -t1.e-f+f),11)-11) AS integer)))*max(e) from t1)+e* - -a-t1.d*e FROM t1 WHERE coalesce((select max(case when 11 between d and d then e | b else case (select (case count(distinct coalesce((select  -coalesce((select max(t1.f) from t1 where t1.b in (select 11 from t1 union select 19 from t1)),t1.e) from t1 where 13<>17),17)) when (max( -t1.e)) then max(17)+max(11) else ((count(*))) end)*(count(*)) from t1) when 11 then  -13+19 else t1.e end end) from t1 where ((t1.a not between d and  -t1.c))),t1.a)<11}
SELECT ~case when t1.e<=t1.a then b else (11) end+(select  -((cast(avg(coalesce((select a from t1 where (select cast(avg(t1.d) AS integer) | case case min((t1.e)) when cast(avg(t1.b) AS integer) then max(t1.c) else max(19) end*count(*) when  -count(distinct e) then count(*) else max( - -17) end from t1) not between c and  -t1.e-f+f),11)-11) AS integer)))*max(e) from t1)+e* - -a-t1.d*e FROM t1 WHERE coalesce((select max(case when 11 between d and d then e | b else case (select (case count(distinct coalesce((select  -coalesce((select max(t1.f) from t1 where t1.b in (select 11 from t1 union select 19 from t1)),t1.e) from t1 where 13<>17),17)) when (max( -t1.e)) then max(17)+max(11) else ((count(*))) end)*(count(*)) from t1) when 11 then  -13+19 else t1.e end end) from t1 where ((t1.a not between d and  -t1.c))),t1.a)<11