-- randexpr1.test
-- 
-- db eval {SELECT case when t1.a<(abs((abs(13)/abs(case t1.e+11*17*case +case case when ~19>=t1.c then f when f not in (a,e,d) then 19 else b end when 19 then 19 else b end &  -c when c then t1.c else c end*d-t1.e when e then 13 else c end)))/abs(t1.b)) then 17 when t1.b not in (t1.a,13,e) then e else  -t1.c end FROM t1 WHERE 19>=11}
SELECT case when t1.a<(abs((abs(13)/abs(case t1.e+11*17*case +case case when ~19>=t1.c then f when f not in (a,e,d) then 19 else b end when 19 then 19 else b end &  -c when c then t1.c else c end*d-t1.e when e then 13 else c end)))/abs(t1.b)) then 17 when t1.b not in (t1.a,13,e) then e else  -t1.c end FROM t1 WHERE 19>=11