-- randexpr1.test
-- 
-- db eval {SELECT  -case when coalesce((select max(case when ~(+case when t1.b*t1.b>t1.e then t1.a when  -d=t1.e or t1.c not between t1.c and e then c else t1.d end)- -17 in (select a from t1 union select  -t1.e from t1) then 19 else a end) from t1 where t1.a in (select  -a from t1 union select 19 from t1)),t1.a)<>t1.d then 13 when a in (19,d,c) and a<11 then t1.a else b end+t1.a-t1.b FROM t1 WHERE c>=t1.e}
SELECT  -case when coalesce((select max(case when ~(+case when t1.b*t1.b>t1.e then t1.a when  -d=t1.e or t1.c not between t1.c and e then c else t1.d end)- -17 in (select a from t1 union select  -t1.e from t1) then 19 else a end) from t1 where t1.a in (select  -a from t1 union select 19 from t1)),t1.a)<>t1.d then 13 when a in (19,d,c) and a<11 then t1.a else b end+t1.a-t1.b FROM t1 WHERE c>=t1.e