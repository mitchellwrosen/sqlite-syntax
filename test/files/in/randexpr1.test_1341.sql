-- randexpr1.test
-- 
-- db eval {SELECT b | t1.d- -coalesce((select case coalesce((select max((select (abs(cast(avg(coalesce((select max(coalesce((select 17 from t1 where  -11=t1.e),a)) from t1 where not exists(select 1 from t1 where 13 between t1.f and 13)),11)-c) AS integer)))+min( -b) from t1)+b) from t1 where t1.d not in (b*19,t1.a,a)),t1.e) when t1.e then t1.b else t1.d end from t1 where (e not in (b,t1.a,(a)))),13) FROM t1 WHERE NOT (t1.c=coalesce((select t1.d*(select count(*) from t1) from t1 where ((abs(e)/abs(case 13 when t1.d then t1.e else 11 end+13))>t1.d) or (t1.f in (select 11 from t1 union select t1.c from t1) or c>t1.e and b not in (t1.b, -t1.e,t1.a)) or c<>b),(a))-19*t1.c-c*t1.d and exists(select 1 from t1 where not exists(select 1 from t1 where t1.a in (t1.b,e,19) or not exists(select 1 from t1 where t1.c=19))))}
SELECT b | t1.d- -coalesce((select case coalesce((select max((select (abs(cast(avg(coalesce((select max(coalesce((select 17 from t1 where  -11=t1.e),a)) from t1 where not exists(select 1 from t1 where 13 between t1.f and 13)),11)-c) AS integer)))+min( -b) from t1)+b) from t1 where t1.d not in (b*19,t1.a,a)),t1.e) when t1.e then t1.b else t1.d end from t1 where (e not in (b,t1.a,(a)))),13) FROM t1 WHERE NOT (t1.c=coalesce((select t1.d*(select count(*) from t1) from t1 where ((abs(e)/abs(case 13 when t1.d then t1.e else 11 end+13))>t1.d) or (t1.f in (select 11 from t1 union select t1.c from t1) or c>t1.e and b not in (t1.b, -t1.e,t1.a)) or c<>b),(a))-19*t1.c-c*t1.d and exists(select 1 from t1 where not exists(select 1 from t1 where t1.a in (t1.b,e,19) or not exists(select 1 from t1 where t1.c=19))))