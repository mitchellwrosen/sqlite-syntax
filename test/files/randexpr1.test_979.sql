-- randexpr1.test
-- 
-- db eval {SELECT case when (13 in (select ~t1.c from t1 union select case when t1.a in (select t1.f-t1.e | t1.c+19 from t1 union select coalesce((select max(13) from t1 where e in (select ~19-(select case count(*)-cast(avg(c) AS integer) when cast(avg(t1.f) AS integer) then  -max(d) else min(t1.d) end | ( -cast(avg(t1.a) AS integer)) from t1)-b*d-coalesce((select max(f) from t1 where not t1.c=c),b) from t1 union select 13 from t1)),a) from t1) then t1.c else t1.b end from t1)) then (abs(c)/abs(19)) else c end FROM t1 WHERE NOT (coalesce((select  -case (select count(*) from t1) when d*+b then a else d end from t1 where b not in (11*case t1.e when case +coalesce((select b+19*+case t1.c when t1.a then t1.b else t1.f end*f*11 from t1 where t1.a=t1.f),c) when t1.e then 17 else d end then t1.f else t1.b end,f,d)),t1.d) in (13,e, -b))}
SELECT case when (13 in (select ~t1.c from t1 union select case when t1.a in (select t1.f-t1.e | t1.c+19 from t1 union select coalesce((select max(13) from t1 where e in (select ~19-(select case count(*)-cast(avg(c) AS integer) when cast(avg(t1.f) AS integer) then  -max(d) else min(t1.d) end | ( -cast(avg(t1.a) AS integer)) from t1)-b*d-coalesce((select max(f) from t1 where not t1.c=c),b) from t1 union select 13 from t1)),a) from t1) then t1.c else t1.b end from t1)) then (abs(c)/abs(19)) else c end FROM t1 WHERE NOT (coalesce((select  -case (select count(*) from t1) when d*+b then a else d end from t1 where b not in (11*case t1.e when case +coalesce((select b+19*+case t1.c when t1.a then t1.b else t1.f end*f*11 from t1 where t1.a=t1.f),c) when t1.e then 17 else d end then t1.f else t1.b end,f,d)),t1.d) in (13,e, -b))