-- randexpr1.test
-- 
-- db eval {SELECT case when (((abs(11 | e)/abs(17-d))+t1.b | a<>11)) and t1.d in (13,t1.c,11) or 17 between t1.f and b or not f in (select 19 from t1 union select t1.e from t1) and b<>19 or e between 19 and t1.d or b<=d or d<17 or t1.a<>t1.f then 17 when t1.e=t1.b then 11 else coalesce((select t1.e from t1 where (b) between  - -d and e),t1.b) end FROM t1 WHERE t1.a between d and +17-17*coalesce((select ~ -19-13-19 from t1 where 19>(select abs(count(distinct t1.d-17))+count(distinct case when 19>17 then t1.f else t1.e end+b) | max(t1.b)+count(distinct 13) from t1)-coalesce((select 13 from t1 where t1.e not in (19,17,e) and 11<=t1.d),c)),t1.f)}
SELECT case when (((abs(11 | e)/abs(17-d))+t1.b | a<>11)) and t1.d in (13,t1.c,11) or 17 between t1.f and b or not f in (select 19 from t1 union select t1.e from t1) and b<>19 or e between 19 and t1.d or b<=d or d<17 or t1.a<>t1.f then 17 when t1.e=t1.b then 11 else coalesce((select t1.e from t1 where (b) between  - -d and e),t1.b) end FROM t1 WHERE t1.a between d and +17-17*coalesce((select ~ -19-13-19 from t1 where 19>(select abs(count(distinct t1.d-17))+count(distinct case when 19>17 then t1.f else t1.e end+b) | max(t1.b)+count(distinct 13) from t1)-coalesce((select 13 from t1 where t1.e not in (19,17,e) and 11<=t1.d),c)),t1.f)