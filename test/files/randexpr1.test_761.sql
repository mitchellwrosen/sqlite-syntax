-- randexpr1.test
-- 
-- db eval {SELECT t1.e*t1.c*(select count(*)+abs(cast(avg(case t1.a when (select  -count(*)-(min(t1.a)) | +(abs(case max(a) when abs(min(17)) then +cast(avg(a) AS integer) | count(distinct d) else count(*) end)) from t1) then a else  -+b-coalesce((select max(f*19) from t1 where ~~c in (a,13,t1.a)),t1.b) end) AS integer)) from t1) FROM t1 WHERE 19-(abs(b)/abs(t1.b)) | case when t1.e in (select case (+cast(avg(t1.c+a) AS integer) | (cast(avg(t1.d) AS integer) |  -cast(avg( -d) AS integer)* -( -count(*))+min(17))) when min(13) then max( -11) else count(*) end from t1 union select min(17) from t1) or 13 in (case when t1.e between e and coalesce((select c from t1 where not exists(select 1 from t1 where (b)>=t1.e)),t1.b) then 19 else 11 end,17,f) then 11 else b end in (select c from t1 union select 19 from t1)}
SELECT t1.e*t1.c*(select count(*)+abs(cast(avg(case t1.a when (select  -count(*)-(min(t1.a)) | +(abs(case max(a) when abs(min(17)) then +cast(avg(a) AS integer) | count(distinct d) else count(*) end)) from t1) then a else  -+b-coalesce((select max(f*19) from t1 where ~~c in (a,13,t1.a)),t1.b) end) AS integer)) from t1) FROM t1 WHERE 19-(abs(b)/abs(t1.b)) | case when t1.e in (select case (+cast(avg(t1.c+a) AS integer) | (cast(avg(t1.d) AS integer) |  -cast(avg( -d) AS integer)* -( -count(*))+min(17))) when min(13) then max( -11) else count(*) end from t1 union select min(17) from t1) or 13 in (case when t1.e between e and coalesce((select c from t1 where not exists(select 1 from t1 where (b)>=t1.e)),t1.b) then 19 else 11 end,17,f) then 11 else b end in (select c from t1 union select 19 from t1)