-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select max(coalesce((select max(11) from t1 where t1.b< -t1.b),e)+coalesce((select t1.b from t1 where not case when ~case when  -17+t1.d> - -t1.d then t1.b when t1.b between 13 and c or t1.a>= -f then t1.b else t1.e end+t1.c<= -c then 19 else t1.f end not in (c,d,e) or t1.f not between t1.b and 19 and 13>= -t1.f or 13 in (e,(t1.b),11)),t1.b)) from t1 where  -(f) not in (f,(t1.e),b)),t1.f) FROM t1 WHERE NOT (f in (select f from t1 union select t1.f*t1.b from t1))}
SELECT coalesce((select max(coalesce((select max(11) from t1 where t1.b< -t1.b),e)+coalesce((select t1.b from t1 where not case when ~case when  -17+t1.d> - -t1.d then t1.b when t1.b between 13 and c or t1.a>= -f then t1.b else t1.e end+t1.c<= -c then 19 else t1.f end not in (c,d,e) or t1.f not between t1.b and 19 and 13>= -t1.f or 13 in (e,(t1.b),11)),t1.b)) from t1 where  -(f) not in (f,(t1.e),b)),t1.f) FROM t1 WHERE NOT (f in (select f from t1 union select t1.f*t1.b from t1))