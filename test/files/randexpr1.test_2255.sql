-- randexpr1.test
-- 
-- db eval {SELECT d-b-(abs(a)/abs(coalesce((select b from t1 where coalesce((select t1.f from t1 where 13>=f-b),t1.c)*case when coalesce((select case (d*t1.e+b) when t1.a-13 then  - -t1.d else t1.a end+13 from t1 where e>=t1.f),11) | b<t1.e then d else c end between c and d),11)-19))+11 FROM t1 WHERE NOT (d>=case when coalesce((select max(case when f<>~b | coalesce((select max(11*c) from t1 where 17=t1.f),t1.d)*d then b when a in (c,(t1.d),(f)) then a else 19 end-t1.f) from t1 where not 13 in (select cast(avg(t1.b) AS integer)-cast(avg( -t1.a) AS integer) from t1 union select count(*) from t1)),17) not in ((t1.f),c, -19) and e between 11 and (19) then 19 else t1.d end)}
SELECT d-b-(abs(a)/abs(coalesce((select b from t1 where coalesce((select t1.f from t1 where 13>=f-b),t1.c)*case when coalesce((select case (d*t1.e+b) when t1.a-13 then  - -t1.d else t1.a end+13 from t1 where e>=t1.f),11) | b<t1.e then d else c end between c and d),11)-19))+11 FROM t1 WHERE NOT (d>=case when coalesce((select max(case when f<>~b | coalesce((select max(11*c) from t1 where 17=t1.f),t1.d)*d then b when a in (c,(t1.d),(f)) then a else 19 end-t1.f) from t1 where not 13 in (select cast(avg(t1.b) AS integer)-cast(avg( -t1.a) AS integer) from t1 union select count(*) from t1)),17) not in ((t1.f),c, -19) and e between 11 and (19) then 19 else t1.d end)