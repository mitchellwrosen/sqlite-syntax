-- randexpr1.test
-- 
-- db eval {SELECT t1.f+coalesce((select t1.a-t1.d*f-coalesce((select max(coalesce((select max(t1.c) from t1 where exists(select 1 from t1 where a=~t1.d+case when (c in (select abs(~( -cast(avg(e) AS integer))) from t1 union select  -min(19) from t1)) and 19<>t1.e or t1.e<11 and c in (t1.b,t1.b,t1.d) or 17 between  -t1.d and t1.d then 11 else d end)),c-f)) from t1 where t1.a<=t1.c),11)-t1.c from t1 where f between t1.a and d),19) FROM t1 WHERE exists(select 1 from t1 where case +t1.d when d then f else t1.e end in (t1.e,17,11) or 13*e-t1.b in (select t1.e from t1 union select 17 from t1)) and 11<=case  -case when not exists(select 1 from t1 where (17 | t1.e in (11,11,11) and b<(19))) then case t1.c+t1.f+c when t1.b then c else 19 end else 17 end when d then t1.b else t1.a end}
SELECT t1.f+coalesce((select t1.a-t1.d*f-coalesce((select max(coalesce((select max(t1.c) from t1 where exists(select 1 from t1 where a=~t1.d+case when (c in (select abs(~( -cast(avg(e) AS integer))) from t1 union select  -min(19) from t1)) and 19<>t1.e or t1.e<11 and c in (t1.b,t1.b,t1.d) or 17 between  -t1.d and t1.d then 11 else d end)),c-f)) from t1 where t1.a<=t1.c),11)-t1.c from t1 where f between t1.a and d),19) FROM t1 WHERE exists(select 1 from t1 where case +t1.d when d then f else t1.e end in (t1.e,17,11) or 13*e-t1.b in (select t1.e from t1 union select 17 from t1)) and 11<=case  -case when not exists(select 1 from t1 where (17 | t1.e in (11,11,11) and b<(19))) then case t1.c+t1.f+c when t1.b then c else 19 end else 17 end when d then t1.b else t1.a end