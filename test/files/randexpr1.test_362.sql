-- randexpr1.test
-- 
-- db eval {SELECT case a when ~c then e+17-d else case t1.a when c then t1.b else case when ((case t1.e when c then d-11 else t1.f end not in (e,(17),t1.d))) or not exists(select 1 from t1 where e not in (d,t1.f,11) and  -t1.a>t1.b) or t1.a not in (f,t1.b,t1.e) or t1.d<=t1.b and 19 not in (t1.d,11,t1.f) then t1.d else 19 end+e+17 end |  -t1.c*d end-13 FROM t1 WHERE e not in (e,t1.b,t1.d)}
SELECT case a when ~c then e+17-d else case t1.a when c then t1.b else case when ((case t1.e when c then d-11 else t1.f end not in (e,(17),t1.d))) or not exists(select 1 from t1 where e not in (d,t1.f,11) and  -t1.a>t1.b) or t1.a not in (f,t1.b,t1.e) or t1.d<=t1.b and 19 not in (t1.d,11,t1.f) then t1.d else 19 end+e+17 end |  -t1.c*d end-13 FROM t1 WHERE e not in (e,t1.b,t1.d)