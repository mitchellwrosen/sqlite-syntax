-- randexpr1.test
-- 
-- db eval {SELECT f+coalesce((select case when ~case when coalesce((select max(b) from t1 where not exists(select 1 from t1 where 11*t1.b>case t1.b when 13 then 11 else b end)),t1.b) not in (a,b,+t1.e & case when t1.a<>17 then 17 else t1.e end) then c when (t1.e)<>d then 11 else f end*(e) not between d and e then 17 else 17 end from t1 where 13 not between 19 and f),t1.b)*11 FROM t1 WHERE NOT (not exists(select 1 from t1 where 11 between c and b and  -case when 13 between  -f | case when t1.b<+c then b when t1.f>13 then a else (d) end and d then t1.d when t1.a in (select 19 from t1 union select 11 from t1) or not exists(select 1 from t1 where exists(select 1 from t1 where not b=t1.b)) then  -19 else t1.b end in (select max(17) from t1 union select count(distinct a)+count(distinct 11) from t1) and t1.f<t1.d or b not in (d,t1.d,a)))}
SELECT f+coalesce((select case when ~case when coalesce((select max(b) from t1 where not exists(select 1 from t1 where 11*t1.b>case t1.b when 13 then 11 else b end)),t1.b) not in (a,b,+t1.e & case when t1.a<>17 then 17 else t1.e end) then c when (t1.e)<>d then 11 else f end*(e) not between d and e then 17 else 17 end from t1 where 13 not between 19 and f),t1.b)*11 FROM t1 WHERE NOT (not exists(select 1 from t1 where 11 between c and b and  -case when 13 between  -f | case when t1.b<+c then b when t1.f>13 then a else (d) end and d then t1.d when t1.a in (select 19 from t1 union select 11 from t1) or not exists(select 1 from t1 where exists(select 1 from t1 where not b=t1.b)) then  -19 else t1.b end in (select max(17) from t1 union select count(distinct a)+count(distinct 11) from t1) and t1.f<t1.d or b not in (d,t1.d,a)))