-- randexpr1.test
-- 
-- db eval {SELECT +17+(11)-coalesce((select max(case (11) when f then +e+case when +11< -13-f or b between (b) and t1.b and e<>t1.c and e not between 13 and t1.d and t1.e in (t1.d,17,t1.c) then c+t1.e*t1.d when t1.f>=t1.d then t1.a else t1.c end else t1.e end-e) from t1 where t1.b not in (t1.b,t1.d,d)), -c)*13 FROM t1 WHERE (abs(e+17)/abs((select count(distinct 11-t1.b)*cast(avg(case c when coalesce((select max(19-coalesce((select max(f) from t1 where 19 in (17,t1.f,11)),t1.d)) from t1 where c in (t1.a,11,13) and t1.a<=t1.c),t1.c) then t1.c else e end) AS integer) | max(e) | (+~case case cast(avg(t1.d) AS integer) when  -min(13) then count(*) else count(*) end when max(t1.c) then count(distinct f) else count(*) end-count(distinct 19)) from t1)))>=t1.e | f}
SELECT +17+(11)-coalesce((select max(case (11) when f then +e+case when +11< -13-f or b between (b) and t1.b and e<>t1.c and e not between 13 and t1.d and t1.e in (t1.d,17,t1.c) then c+t1.e*t1.d when t1.f>=t1.d then t1.a else t1.c end else t1.e end-e) from t1 where t1.b not in (t1.b,t1.d,d)), -c)*13 FROM t1 WHERE (abs(e+17)/abs((select count(distinct 11-t1.b)*cast(avg(case c when coalesce((select max(19-coalesce((select max(f) from t1 where 19 in (17,t1.f,11)),t1.d)) from t1 where c in (t1.a,11,13) and t1.a<=t1.c),t1.c) then t1.c else e end) AS integer) | max(e) | (+~case case cast(avg(t1.d) AS integer) when  -min(13) then count(*) else count(*) end when max(t1.c) then count(distinct f) else count(*) end-count(distinct 19)) from t1)))>=t1.e | f