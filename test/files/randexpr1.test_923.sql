-- randexpr1.test
-- 
-- db eval {SELECT (select  -abs(max(+(select +count(*) from t1)))-max(case when b in (select (abs(13)/abs(b)) from t1 union select case when exists(select 1 from t1 where not exists(select 1 from t1 where (abs(17)/abs(coalesce((select max(+19*t1.e+case t1.f when (abs(f)/abs(t1.f)) then t1.f else e end) from t1 where 19<17),t1.a)))+11 in (select b from t1 union select t1.e from t1))) then d else t1.c end from t1) then f when 13=d then  -a else t1.a end) from t1) FROM t1 WHERE NOT (~(select ~ -(min(f)- -cast(avg(f) AS integer)-abs(count(distinct case when t1.d=b then d when e in (select +cast(avg(e) AS integer) from t1 union select count(*) from t1) then t1.f else c end*17*f))) | (count(*)) from t1)-f<>case when (select + -max(t1.e) from t1)+(select  -min(17) from t1)*13 between 17 and 19 then t1.b else t1.d end)}
SELECT (select  -abs(max(+(select +count(*) from t1)))-max(case when b in (select (abs(13)/abs(b)) from t1 union select case when exists(select 1 from t1 where not exists(select 1 from t1 where (abs(17)/abs(coalesce((select max(+19*t1.e+case t1.f when (abs(f)/abs(t1.f)) then t1.f else e end) from t1 where 19<17),t1.a)))+11 in (select b from t1 union select t1.e from t1))) then d else t1.c end from t1) then f when 13=d then  -a else t1.a end) from t1) FROM t1 WHERE NOT (~(select ~ -(min(f)- -cast(avg(f) AS integer)-abs(count(distinct case when t1.d=b then d when e in (select +cast(avg(e) AS integer) from t1 union select count(*) from t1) then t1.f else c end*17*f))) | (count(*)) from t1)-f<>case when (select + -max(t1.e) from t1)+(select  -min(17) from t1)*13 between 17 and 19 then t1.b else t1.d end)