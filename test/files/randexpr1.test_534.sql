-- randexpr1.test
-- 
-- db eval {SELECT (select ~abs(max(case when (case when ( -t1.a>e) then t1.a else (select (~count(*) | max(case when t1.d<>c then  -+13 else case when b between 19 and 17 or (t1.a)>=13 then 19 else t1.c end end)) from t1) end<>t1.e) then 19 when c*e+t1.b<=19 then f else (t1.d) end | b)) from t1)+c FROM t1 WHERE NOT (f | t1.a+b>=t1.d)}
SELECT (select ~abs(max(case when (case when ( -t1.a>e) then t1.a else (select (~count(*) | max(case when t1.d<>c then  -+13 else case when b between 19 and 17 or (t1.a)>=13 then 19 else t1.c end end)) from t1) end<>t1.e) then 19 when c*e+t1.b<=19 then f else (t1.d) end | b)) from t1)+c FROM t1 WHERE NOT (f | t1.a+b>=t1.d)