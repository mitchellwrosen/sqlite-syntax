-- randexpr1.test
-- 
-- db eval {SELECT 11 | ~case case when e between 17 and coalesce((select max(((f-t1.f))) from t1 where d<>f),t1.f) and 11 not between 13 and 13 and ( -11<>c and t1.b>c) or 19<>t1.e and t1.b<=t1.f then +f*b when t1.d>=e then  -13 else t1.c end when b then (17) else t1.d end+a+f*a FROM t1 WHERE not exists(select 1 from t1 where a between t1.f and 17 or e>=(d)*19+t1.e*c)}
SELECT 11 | ~case case when e between 17 and coalesce((select max(((f-t1.f))) from t1 where d<>f),t1.f) and 11 not between 13 and 13 and ( -11<>c and t1.b>c) or 19<>t1.e and t1.b<=t1.f then +f*b when t1.d>=e then  -13 else t1.c end when b then (17) else t1.d end+a+f*a FROM t1 WHERE not exists(select 1 from t1 where a between t1.f and 17 or e>=(d)*19+t1.e*c)