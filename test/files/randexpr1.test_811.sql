-- randexpr1.test
-- 
-- db eval {SELECT case (abs(f)/abs(f)) when t1.e*t1.f then (abs(t1.d)/abs(t1.b*11)) else ~case when case ~~19 when t1.f then e else case coalesce((select (abs(f)/abs(a)) from t1 where t1.a<>17 and t1.a>= -t1.b),t1.f) when t1.d then c else t1.d end end in (select count(*) from t1 union select ~abs(min(t1.a)) & count(*) from t1) then 17 when t1.f not between (e) and t1.c then f else b end+c+a end FROM t1 WHERE NOT (t1.f in (case when ~(select max(d) from t1)*t1.f>=case when 17*d<t1.f then d when (t1.d) not between t1.e-t1.d-e and +(t1.e*13*(select case count(*) when  -count(distinct e) then max(13) else count(distinct t1.b) end from t1)-case when c<e then c else 11 end)-17 then  -c else t1.c end then e else (13) end+d,t1.a,11))}
SELECT case (abs(f)/abs(f)) when t1.e*t1.f then (abs(t1.d)/abs(t1.b*11)) else ~case when case ~~19 when t1.f then e else case coalesce((select (abs(f)/abs(a)) from t1 where t1.a<>17 and t1.a>= -t1.b),t1.f) when t1.d then c else t1.d end end in (select count(*) from t1 union select ~abs(min(t1.a)) & count(*) from t1) then 17 when t1.f not between (e) and t1.c then f else b end+c+a end FROM t1 WHERE NOT (t1.f in (case when ~(select max(d) from t1)*t1.f>=case when 17*d<t1.f then d when (t1.d) not between t1.e-t1.d-e and +(t1.e*13*(select case count(*) when  -count(distinct e) then max(13) else count(distinct t1.b) end from t1)-case when c<e then c else 11 end)-17 then  -c else t1.c end then e else (13) end+d,t1.a,11))