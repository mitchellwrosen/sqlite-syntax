-- randexpr1.test
-- 
-- db eval {SELECT 11+case when case case f when (abs(t1.a)/abs(19))+t1.e then (abs(19)/abs(b+e)) else 13 end | 11 when (select ~~+min(t1.e) from t1) then t1.c else  -e end<>case t1.a when 17 then case when (c not between 19 and t1.b) and t1.b<>t1.d then coalesce((select a from t1 where t1.d in (t1.e,t1.f,f)),t1.e) else 13 end else d end or t1.c<t1.d then t1.f else 17 end-f FROM t1 WHERE NOT ((f*c*coalesce((select e from t1 where (exists(select 1 from t1 where +t1.e<= -e | t1.f))),a)+b in (select t1.c from t1 union select c from t1) or (t1.e between b and (19)) and 13 between t1.e and 13 and 11 in (13,t1.c,17) and exists(select 1 from t1 where t1.d in (select count(*)-max(e)-((max((11)))) from t1 union select max(17) from t1)) and c>f))}
SELECT 11+case when case case f when (abs(t1.a)/abs(19))+t1.e then (abs(19)/abs(b+e)) else 13 end | 11 when (select ~~+min(t1.e) from t1) then t1.c else  -e end<>case t1.a when 17 then case when (c not between 19 and t1.b) and t1.b<>t1.d then coalesce((select a from t1 where t1.d in (t1.e,t1.f,f)),t1.e) else 13 end else d end or t1.c<t1.d then t1.f else 17 end-f FROM t1 WHERE NOT ((f*c*coalesce((select e from t1 where (exists(select 1 from t1 where +t1.e<= -e | t1.f))),a)+b in (select t1.c from t1 union select c from t1) or (t1.e between b and (19)) and 13 between t1.e and 13 and 11 in (13,t1.c,17) and exists(select 1 from t1 where t1.d in (select count(*)-max(e)-((max((11)))) from t1 union select max(17) from t1)) and c>f))