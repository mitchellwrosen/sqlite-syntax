-- randexpr1.test
-- 
-- db eval {SELECT case when coalesce((select d from t1 where not coalesce((select f*t1.b from t1 where exists(select 1 from t1 where (t1.f*t1.a)-19-13 in (select ~(cast(avg(t1.b) AS integer)) from t1 union select min(t1.a)+case min( -t1.a) when cast(avg(19) AS integer) then cast(avg(19) AS integer) else min(13) end+( -min(a)) from t1))),(t1.f)) between 17 and d),11)=19 then t1.b when 17<11 or exists(select 1 from t1 where d in (select 13 from t1 union select f from t1)) then 11 else t1.c end FROM t1 WHERE NOT (case c when 13 then +t1.d else t1.a end in (select abs(case + -count(*)+count(distinct coalesce((select (abs(~case when t1.c=19 | 13 then t1.c when not e>(13) then f else d end)/abs(a)) from t1 where 17>=t1.b),t1.b))+min(t1.a)*count(*) | (((max(a)))-cast(avg((17)) AS integer)) when max(d) then count(*) else cast(avg(t1.e) AS integer) end) from t1 union select count(*) from t1))}
SELECT case when coalesce((select d from t1 where not coalesce((select f*t1.b from t1 where exists(select 1 from t1 where (t1.f*t1.a)-19-13 in (select ~(cast(avg(t1.b) AS integer)) from t1 union select min(t1.a)+case min( -t1.a) when cast(avg(19) AS integer) then cast(avg(19) AS integer) else min(13) end+( -min(a)) from t1))),(t1.f)) between 17 and d),11)=19 then t1.b when 17<11 or exists(select 1 from t1 where d in (select 13 from t1 union select f from t1)) then 11 else t1.c end FROM t1 WHERE NOT (case c when 13 then +t1.d else t1.a end in (select abs(case + -count(*)+count(distinct coalesce((select (abs(~case when t1.c=19 | 13 then t1.c when not e>(13) then f else d end)/abs(a)) from t1 where 17>=t1.b),t1.b))+min(t1.a)*count(*) | (((max(a)))-cast(avg((17)) AS integer)) when max(d) then count(*) else cast(avg(t1.e) AS integer) end) from t1 union select count(*) from t1))