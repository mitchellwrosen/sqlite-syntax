-- selectA.test
-- 
-- execsql {
--     SELECT x,y,z FROM t2 UNION ALL SELECT a,b,c FROM t1
--     ORDER BY c,a,b
-- }
SELECT x,y,z FROM t2 UNION ALL SELECT a,b,c FROM t1
ORDER BY c,a,b