-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select 11 from t1 where exists(select 1 from t1 where not exists(select 1 from t1 where not (select +count(*)* -count(distinct case when exists(select 1 from t1 where 13+a=t1.b) then d else 13 end-t1.b) from t1)<=17 & e-13)) or not exists(select 1 from t1 where not t1.e in ((13),17,19) and not t1.d>a and t1.e not between t1.e and b)),coalesce((select max(coalesce((select max(b) from t1 where (b)>11),d)) from t1 where 11> -19),f)-c) FROM t1 WHERE case when  -13 not in (b,(abs(coalesce((select max(case when  -17<>t1.f then b when coalesce((select t1.b from t1 where t1.c in (select (a) from t1 union select c from t1)),17)<=t1.e then b else f end) from t1 where b<=11),t1.c))/abs((d))),19) then t1.b when (exists(select 1 from t1 where not exists(select 1 from t1 where ((13)=e and t1.f between 17 and t1.a) or t1.f between (a) and  -(t1.f)))) and t1.e between b and 13 or b<>(f) then t1.f else d end>=a or d>=13}
SELECT coalesce((select 11 from t1 where exists(select 1 from t1 where not exists(select 1 from t1 where not (select +count(*)* -count(distinct case when exists(select 1 from t1 where 13+a=t1.b) then d else 13 end-t1.b) from t1)<=17 & e-13)) or not exists(select 1 from t1 where not t1.e in ((13),17,19) and not t1.d>a and t1.e not between t1.e and b)),coalesce((select max(coalesce((select max(b) from t1 where (b)>11),d)) from t1 where 11> -19),f)-c) FROM t1 WHERE case when  -13 not in (b,(abs(coalesce((select max(case when  -17<>t1.f then b when coalesce((select t1.b from t1 where t1.c in (select (a) from t1 union select c from t1)),17)<=t1.e then b else f end) from t1 where b<=11),t1.c))/abs((d))),19) then t1.b when (exists(select 1 from t1 where not exists(select 1 from t1 where ((13)=e and t1.f between 17 and t1.a) or t1.f between (a) and  -(t1.f)))) and t1.e between b and 13 or b<>(f) then t1.f else d end>=a or d>=13