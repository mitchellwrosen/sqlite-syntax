-- randexpr1.test
-- 
-- db eval {SELECT (coalesce((select case when exists(select 1 from t1 where case when (t1.b)>=f+e*coalesce((select max( -+t1.c) from t1 where case a when e then e else 11 end in (select 17 from t1 union select d from t1) or e not between d and a),t1.f) then ( -t1.d) else e end in (11,c,b)) then 17 else t1.f end+t1.f from t1 where not d in ((b),(f),11) and t1.f<f),19)) FROM t1 WHERE exists(select 1 from t1 where coalesce((select max(case when t1.d in (select  -a from t1 union select 11 from t1) then t1.b when case when t1.f not in (t1.d, -19,t1.e) and t1.a>=t1.b and 19<13 then 19 else 13 end<a then e else b end) from t1 where t1.f in (select +count(distinct  - -13)* - -case max(t1.f) when max(13) then (min(t1.c)) else (max(t1.b)) end from t1 union select cast(avg(t1.a) AS integer) from t1) or t1.e not in (b,17,e)), -17)>=19) and b not in (c,t1.a,d) or b between e and  -t1.f or t1.b between t1.e and c and t1.a<=t1.d}
SELECT (coalesce((select case when exists(select 1 from t1 where case when (t1.b)>=f+e*coalesce((select max( -+t1.c) from t1 where case a when e then e else 11 end in (select 17 from t1 union select d from t1) or e not between d and a),t1.f) then ( -t1.d) else e end in (11,c,b)) then 17 else t1.f end+t1.f from t1 where not d in ((b),(f),11) and t1.f<f),19)) FROM t1 WHERE exists(select 1 from t1 where coalesce((select max(case when t1.d in (select  -a from t1 union select 11 from t1) then t1.b when case when t1.f not in (t1.d, -19,t1.e) and t1.a>=t1.b and 19<13 then 19 else 13 end<a then e else b end) from t1 where t1.f in (select +count(distinct  - -13)* - -case max(t1.f) when max(13) then (min(t1.c)) else (max(t1.b)) end from t1 union select cast(avg(t1.a) AS integer) from t1) or t1.e not in (b,17,e)), -17)>=19) and b not in (c,t1.a,d) or b between e and  -t1.f or t1.b between t1.e and c and t1.a<=t1.d