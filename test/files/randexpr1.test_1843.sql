-- randexpr1.test
-- 
-- db eval {SELECT 17 |  -(case when (a) not between 11 and case (abs(~(abs(13-case (abs(17)/abs(19+e | t1.e)) when (t1.d) then  -(t1.c) else t1.f end+t1.f*t1.c)/abs(a)))/abs(17)) when 19 then d else t1.d end then 17 when not exists(select 1 from t1 where t1.e not in (t1.b,t1.b,b) or a between a and 17) then 11 else b end) | t1.e FROM t1 WHERE NOT (17>=e)}
SELECT 17 |  -(case when (a) not between 11 and case (abs(~(abs(13-case (abs(17)/abs(19+e | t1.e)) when (t1.d) then  -(t1.c) else t1.f end+t1.f*t1.c)/abs(a)))/abs(17)) when 19 then d else t1.d end then 17 when not exists(select 1 from t1 where t1.e not in (t1.b,t1.b,b) or a between a and 17) then 11 else b end) | t1.e FROM t1 WHERE NOT (17>=e)