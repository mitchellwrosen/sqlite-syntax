-- randexpr1.test
-- 
-- db eval {SELECT +t1.b-17*11-coalesce((select coalesce((select max(t1.f+a-t1.c+b) from t1 where 17* -b+t1.e=13),coalesce((select +t1.f*coalesce((select b from t1 where 19 between  -a and c),t1.d) from t1 where 17<>b),17)*11) from t1 where f<c),e) |  -t1.e-b-t1.b-t1.e FROM t1 WHERE t1.b=11}
SELECT +t1.b-17*11-coalesce((select coalesce((select max(t1.f+a-t1.c+b) from t1 where 17* -b+t1.e=13),coalesce((select +t1.f*coalesce((select b from t1 where 19 between  -a and c),t1.d) from t1 where 17<>b),17)*11) from t1 where f<c),e) |  -t1.e-b-t1.b-t1.e FROM t1 WHERE t1.b=11