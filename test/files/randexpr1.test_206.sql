-- randexpr1.test
-- 
-- db eval {SELECT  -~d*19-a-c | t1.c+(abs(case  -19 | ~case t1.d when c then t1.f else c end when +t1.d then 17 else case case when b>case case when t1.c=d or  -t1.a=17 then ((a)) when t1.a<>d then (t1.c) else a end+19+11 when 19 then t1.a else b end-(t1.e) then f else t1.c end when 17 then t1.b else a end end)/abs(d))+t1.b FROM t1 WHERE NOT (t1.f+13<>19)}
SELECT  -~d*19-a-c | t1.c+(abs(case  -19 | ~case t1.d when c then t1.f else c end when +t1.d then 17 else case case when b>case case when t1.c=d or  -t1.a=17 then ((a)) when t1.a<>d then (t1.c) else a end+19+11 when 19 then t1.a else b end-(t1.e) then f else t1.c end when 17 then t1.b else a end end)/abs(d))+t1.b FROM t1 WHERE NOT (t1.f+13<>19)