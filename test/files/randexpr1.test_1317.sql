-- randexpr1.test
-- 
-- db eval {SELECT case when t1.b not in (17 | t1.e*t1.a,11,(select +case max((select  -+case (max(t1.f)) when count(*) then count(distinct d) else  -( -(cast(avg(f) AS integer))) end-count(distinct t1.a) from t1)) | count(distinct (t1.e)) when  -min(c) then count(*) else  -count(distinct e) end | cast(avg(17) AS integer) from t1)) then case when d<=t1.c and (exists(select 1 from t1 where (select count(distinct (e)) from t1)=(abs(17)/abs(d)))) then (abs(t1.f)/abs(t1.c)) when t1.f<>e or t1.c<=d and t1.e<=t1.d then 19 else  - -d end-t1.e else (t1.f) end FROM t1 WHERE NOT (not t1.b=coalesce((select (abs(c)/abs(11)) from t1 where exists(select 1 from t1 where  -coalesce((select max(((abs(coalesce((select max(case (select min((abs(19)/abs(19))+19)*count(*)- -cast(avg(t1.d) AS integer)-max(17)+min( -11) from t1) when e then t1.e else f end) from t1 where t1.d<=t1.f), -f)- -t1.e)/abs(d))-t1.b)) from t1 where c<11),t1.b)*a not in (d,d,f))),c))}
SELECT case when t1.b not in (17 | t1.e*t1.a,11,(select +case max((select  -+case (max(t1.f)) when count(*) then count(distinct d) else  -( -(cast(avg(f) AS integer))) end-count(distinct t1.a) from t1)) | count(distinct (t1.e)) when  -min(c) then count(*) else  -count(distinct e) end | cast(avg(17) AS integer) from t1)) then case when d<=t1.c and (exists(select 1 from t1 where (select count(distinct (e)) from t1)=(abs(17)/abs(d)))) then (abs(t1.f)/abs(t1.c)) when t1.f<>e or t1.c<=d and t1.e<=t1.d then 19 else  - -d end-t1.e else (t1.f) end FROM t1 WHERE NOT (not t1.b=coalesce((select (abs(c)/abs(11)) from t1 where exists(select 1 from t1 where  -coalesce((select max(((abs(coalesce((select max(case (select min((abs(19)/abs(19))+19)*count(*)- -cast(avg(t1.d) AS integer)-max(17)+min( -11) from t1) when e then t1.e else f end) from t1 where t1.d<=t1.f), -f)- -t1.e)/abs(d))-t1.b)) from t1 where c<11),t1.b)*a not in (d,d,f))),c))