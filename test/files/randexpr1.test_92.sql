-- randexpr1.test
-- 
-- db eval {SELECT case when t1.f>t1.d then (select abs(+count(distinct c)+(abs(min((abs(+11)/abs(b)))* -count(*)))) from t1)-t1.a when d+case when (select cast(avg(t1.f) AS integer)-count(distinct 19) from t1)<>17 then c when t1.e in (select cast(avg(t1.a) AS integer) from t1 union select count(distinct f) from t1) and d>t1.c and t1.f not in (11,19,t1.b) then a else (c) end+(17)=c then t1.d else t1.c end-c FROM t1 WHERE NOT (13 not in (e,t1.b,+coalesce((select max(b) from t1 where 13*b-t1.d-case when 11>=a then t1.a-t1.d else (t1.d)*e+19 end*t1.a+19+(11) | t1.c*t1.a-17<t1.e),b)-a+17*17*c))}
SELECT case when t1.f>t1.d then (select abs(+count(distinct c)+(abs(min((abs(+11)/abs(b)))* -count(*)))) from t1)-t1.a when d+case when (select cast(avg(t1.f) AS integer)-count(distinct 19) from t1)<>17 then c when t1.e in (select cast(avg(t1.a) AS integer) from t1 union select count(distinct f) from t1) and d>t1.c and t1.f not in (11,19,t1.b) then a else (c) end+(17)=c then t1.d else t1.c end-c FROM t1 WHERE NOT (13 not in (e,t1.b,+coalesce((select max(b) from t1 where 13*b-t1.d-case when 11>=a then t1.a-t1.d else (t1.d)*e+19 end*t1.a+19+(11) | t1.c*t1.a-17<t1.e),b)-a+17*17*c))