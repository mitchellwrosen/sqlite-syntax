-- randexpr1.test
-- 
-- db eval {SELECT case when (case when (e not in (e | e,17,e)) then 13 else t1.b end>+t1.b*f-b+t1.d-e) or 19 between b and t1.a then 11 when exists(select 1 from t1 where (t1.d) | t1.d in (case when (e>=(11)) or (t1.a)>=t1.f then c-b when t1.c<>t1.e then t1.a else (17) end,c,13)) then 17 else t1.b end FROM t1 WHERE NOT ((select min(case when a not in (coalesce((select 19 from t1 where t1.f<((abs(e)/abs(t1.a+e)) | d)-e),e),t1.f,t1.a) then c else 13 end)-(count(distinct d)-abs(count(distinct t1.c)-cast(avg(d) AS integer) | min(b)+(cast(avg(19) AS integer) | case count(distinct t1.c) when cast(avg( -t1.b) AS integer) then cast(avg(t1.a) AS integer) else max(13) end))) from t1) not between b | a and 19)}
SELECT case when (case when (e not in (e | e,17,e)) then 13 else t1.b end>+t1.b*f-b+t1.d-e) or 19 between b and t1.a then 11 when exists(select 1 from t1 where (t1.d) | t1.d in (case when (e>=(11)) or (t1.a)>=t1.f then c-b when t1.c<>t1.e then t1.a else (17) end,c,13)) then 17 else t1.b end FROM t1 WHERE NOT ((select min(case when a not in (coalesce((select 19 from t1 where t1.f<((abs(e)/abs(t1.a+e)) | d)-e),e),t1.f,t1.a) then c else 13 end)-(count(distinct d)-abs(count(distinct t1.c)-cast(avg(d) AS integer) | min(b)+(cast(avg(19) AS integer) | case count(distinct t1.c) when cast(avg( -t1.b) AS integer) then cast(avg(t1.a) AS integer) else max(13) end))) from t1) not between b | a and 19)