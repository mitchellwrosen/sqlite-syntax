-- tkt1473.test
-- 
-- execsql {
--     SELECT 1 FROM t1 WHERE a=1 UNION ALL SELECT 2 FROM t1 WHERE b=4
-- }
SELECT 1 FROM t1 WHERE a=1 UNION ALL SELECT 2 FROM t1 WHERE b=4