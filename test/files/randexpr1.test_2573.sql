-- randexpr1.test
-- 
-- db eval {SELECT case when (not 19>=(abs(t1.b)/abs(+f))) then case 13 when case t1.c when t1.b then f else 13 end then t1.e else t1.a end+coalesce((select max(c) from t1 where a not between t1.d*17*coalesce((select max(e*b) from t1 where t1.a=11),t1.c) and t1.d),t1.d) when (case when t1.d not in (e,t1.a,t1.b) then 19 else t1.f end between t1.c and 19) then t1.e else 11 end FROM t1 WHERE NOT (t1.b*+c>=17)}
SELECT case when (not 19>=(abs(t1.b)/abs(+f))) then case 13 when case t1.c when t1.b then f else 13 end then t1.e else t1.a end+coalesce((select max(c) from t1 where a not between t1.d*17*coalesce((select max(e*b) from t1 where t1.a=11),t1.c) and t1.d),t1.d) when (case when t1.d not in (e,t1.a,t1.b) then 19 else t1.f end between t1.c and 19) then t1.e else 11 end FROM t1 WHERE NOT (t1.b*+c>=17)