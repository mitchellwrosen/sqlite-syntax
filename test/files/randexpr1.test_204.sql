-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select max(coalesce((select 19 from t1 where (case when ((abs(coalesce((select max(case when t1.a between (t1.c)+ -b and d then e else d end) from t1 where t1.e in (select (count(*)) from t1 union select max( -a)-count(distinct t1.c)-((min(13))) & max((t1.f)) from t1)),11))/abs(19)))+d>c then t1.b when 17 not between t1.e and c then 11 else f end in (select a from t1 union select 13 from t1))),11)) from t1 where not exists(select 1 from t1 where f=17)),e) FROM t1 WHERE NOT (( -13 in (13,b,case when 19=11 then t1.d else case when (d | b-case when t1.f-(select +abs(cast(avg( -17* -17) AS integer)*count(*)+max(t1.e)) from t1)*t1.f not in (f,a,c) then t1.a else t1.b end between 19 and e) then a else 13 end end-t1.a) or t1.c<19))}
SELECT coalesce((select max(coalesce((select 19 from t1 where (case when ((abs(coalesce((select max(case when t1.a between (t1.c)+ -b and d then e else d end) from t1 where t1.e in (select (count(*)) from t1 union select max( -a)-count(distinct t1.c)-((min(13))) & max((t1.f)) from t1)),11))/abs(19)))+d>c then t1.b when 17 not between t1.e and c then 11 else f end in (select a from t1 union select 13 from t1))),11)) from t1 where not exists(select 1 from t1 where f=17)),e) FROM t1 WHERE NOT (( -13 in (13,b,case when 19=11 then t1.d else case when (d | b-case when t1.f-(select +abs(cast(avg( -17* -17) AS integer)*count(*)+max(t1.e)) from t1)*t1.f not in (f,a,c) then t1.a else t1.b end between 19 and e) then a else 13 end end-t1.a) or t1.c<19))