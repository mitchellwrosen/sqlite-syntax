-- randexpr1.test
-- 
-- db eval {SELECT b-case when exists(select 1 from t1 where (exists(select 1 from t1 where t1.a not between 17 and a+t1.f) or not exists(select 1 from t1 where not exists(select 1 from t1 where t1.b+f+a in (select max(19) &  -max(t1.e)-cast(avg(t1.c) AS integer)- -count(distinct t1.b) from t1 union select max(t1.b) from t1)) or not exists(select 1 from t1 where not exists(select 1 from t1 where 11>c or (13) in ( -11,c,19)) or 11>f))) and t1.c not in ( -(d),t1.a,t1.a) and 17 in (t1.b,13,19)) then coalesce((select max(+19) from t1 where 17<b),13) else (abs(11)/abs(t1.e)) end FROM t1 WHERE NOT (not exists(select 1 from t1 where (13 between f and (e) or (f-d in (select t1.c*t1.d | f-19 from t1 union select 19*t1.d*coalesce((select coalesce((select max(t1.d-c*11) from t1 where 17= -t1.a),t1.a)-c from t1 where exists(select 1 from t1 where 17>e)),b) from t1))) or ((t1.d<f))))}
SELECT b-case when exists(select 1 from t1 where (exists(select 1 from t1 where t1.a not between 17 and a+t1.f) or not exists(select 1 from t1 where not exists(select 1 from t1 where t1.b+f+a in (select max(19) &  -max(t1.e)-cast(avg(t1.c) AS integer)- -count(distinct t1.b) from t1 union select max(t1.b) from t1)) or not exists(select 1 from t1 where not exists(select 1 from t1 where 11>c or (13) in ( -11,c,19)) or 11>f))) and t1.c not in ( -(d),t1.a,t1.a) and 17 in (t1.b,13,19)) then coalesce((select max(+19) from t1 where 17<b),13) else (abs(11)/abs(t1.e)) end FROM t1 WHERE NOT (not exists(select 1 from t1 where (13 between f and (e) or (f-d in (select t1.c*t1.d | f-19 from t1 union select 19*t1.d*coalesce((select coalesce((select max(t1.d-c*11) from t1 where 17= -t1.a),t1.a)-c from t1 where exists(select 1 from t1 where 17>e)),b) from t1))) or ((t1.d<f))))