-- randexpr1.test
-- 
-- db eval {SELECT ~f+coalesce((select max(17+~a+t1.b) from t1 where 17>t1.c),c+case when t1.f*d not between t1.f-13 and ~t1.b then 19 when t1.a between case coalesce((select 17 from t1 where (e in (f,e,t1.b))),13) | t1.a* - -c*t1.f-a+a when e then t1.a else 13 end and b then b else e end)-11 FROM t1 WHERE 17=19*~d-case when (exists(select 1 from t1 where 17-e not between t1.b and f) or  -f not in ( -13,b,c) and t1.f not between t1.f and 13) then 13 else coalesce((select t1.f from t1 where 11 not in (a,(e),t1.f)),(t1.c)) end+t1.c or 17 in (select +(count(*)) from t1 union select ~+case  -min(t1.b) when case case max(19) when cast(avg((b)) AS integer) then (count(distinct t1.b)) else  -cast(avg(t1.b) AS integer) end when ( -max(t1.e)) then max(17) else  -count(distinct t1.f) end then cast(avg(19) AS integer) else count(*) end-count(*) |  -cast(avg(11) AS integer) from t1)}
SELECT ~f+coalesce((select max(17+~a+t1.b) from t1 where 17>t1.c),c+case when t1.f*d not between t1.f-13 and ~t1.b then 19 when t1.a between case coalesce((select 17 from t1 where (e in (f,e,t1.b))),13) | t1.a* - -c*t1.f-a+a when e then t1.a else 13 end and b then b else e end)-11 FROM t1 WHERE 17=19*~d-case when (exists(select 1 from t1 where 17-e not between t1.b and f) or  -f not in ( -13,b,c) and t1.f not between t1.f and 13) then 13 else coalesce((select t1.f from t1 where 11 not in (a,(e),t1.f)),(t1.c)) end+t1.c or 17 in (select +(count(*)) from t1 union select ~+case  -min(t1.b) when case case max(19) when cast(avg((b)) AS integer) then (count(distinct t1.b)) else  -cast(avg(t1.b) AS integer) end when ( -max(t1.e)) then max(17) else  -count(distinct t1.f) end then cast(avg(19) AS integer) else count(*) end-count(*) |  -cast(avg(11) AS integer) from t1)