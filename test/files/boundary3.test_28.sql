-- boundary3.test
-- 
-- db eval {
--     SELECT t2.* FROM t1 JOIN t2 USING(a) WHERE x='0000000000004000'
-- }
SELECT t2.* FROM t1 JOIN t2 USING(a) WHERE x='0000000000004000'