-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select 17 from t1 where d between 17 and 13),case when t1.f in (select c+a from t1 union select a from t1) and (abs(f)/abs(t1.b))-t1.e>=case when not exists(select 1 from t1 where t1.e>(select count(distinct t1.f) from t1)) then t1.f when (11)-~t1.f<case 13 when f then 19 else coalesce((select  -t1.c from t1 where 11<=t1.e),e) end then t1.e else 13 end*t1.e then t1.f else b end) FROM t1 WHERE NOT ( -coalesce((select max(11) from t1 where (abs( -case 19 when (abs(coalesce((select max(coalesce((select 13 from t1 where a>e),b)) from t1 where (t1.f) in (13,e,t1.c) or t1.c>13),t1.b))/abs(13)) then t1.b else d end)/abs(13)) in (t1.f,t1.e,t1.a) or 17 between 13 and f or (f>11 and t1.f<t1.b)),case when 19 not between  -17 and d then 11 when t1.c=d then t1.a else t1.f end)>=t1.e)}
SELECT coalesce((select 17 from t1 where d between 17 and 13),case when t1.f in (select c+a from t1 union select a from t1) and (abs(f)/abs(t1.b))-t1.e>=case when not exists(select 1 from t1 where t1.e>(select count(distinct t1.f) from t1)) then t1.f when (11)-~t1.f<case 13 when f then 19 else coalesce((select  -t1.c from t1 where 11<=t1.e),e) end then t1.e else 13 end*t1.e then t1.f else b end) FROM t1 WHERE NOT ( -coalesce((select max(11) from t1 where (abs( -case 19 when (abs(coalesce((select max(coalesce((select 13 from t1 where a>e),b)) from t1 where (t1.f) in (13,e,t1.c) or t1.c>13),t1.b))/abs(13)) then t1.b else d end)/abs(13)) in (t1.f,t1.e,t1.a) or 17 between 13 and f or (f>11 and t1.f<t1.b)),case when 19 not between  -17 and d then 11 when t1.c=d then t1.a else t1.f end)>=t1.e)