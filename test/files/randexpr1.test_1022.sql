-- randexpr1.test
-- 
-- db eval {SELECT t1.a-coalesce((select 11-case when 11<19-f+t1.a then a & (t1.a+++(select cast(avg((abs(t1.d & e)/abs((abs(t1.a)/abs(13))))) AS integer)-max(a) from t1)+e+13*19+t1.a-c) else 13 end+t1.e-19 from t1 where 13>t1.f),e) FROM t1 WHERE ~(abs((select max( -case when d+(11)*d between coalesce((select 11 from t1 where (t1.f)=e),d*f) and t1.e then d-case when a<> -17+(abs(t1.d)/abs(t1.f)) then t1.f else t1.e end*11+c-(c) else d end) from t1)*t1.e)/abs((t1.f)))-f<>11}
SELECT t1.a-coalesce((select 11-case when 11<19-f+t1.a then a & (t1.a+++(select cast(avg((abs(t1.d & e)/abs((abs(t1.a)/abs(13))))) AS integer)-max(a) from t1)+e+13*19+t1.a-c) else 13 end+t1.e-19 from t1 where 13>t1.f),e) FROM t1 WHERE ~(abs((select max( -case when d+(11)*d between coalesce((select 11 from t1 where (t1.f)=e),d*f) and t1.e then d-case when a<> -17+(abs(t1.d)/abs(t1.f)) then t1.f else t1.e end*11+c-(c) else d end) from t1)*t1.e)/abs((t1.f)))-f<>11