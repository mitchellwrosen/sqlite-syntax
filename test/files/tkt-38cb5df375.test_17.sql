-- tkt-38cb5df375.test
-- 
-- execsql {
--       SELECT a FROM (SELECT * FROM t1 ORDER BY a LIMIT 4)
--       UNION ALL SELECT 90+a FROM (SELECT a FROM t1 ORDER BY a LIMIT 3)
--       ORDER BY 1
--       LIMIT ii;
-- }
SELECT a FROM (SELECT * FROM t1 ORDER BY a LIMIT 4)
UNION ALL SELECT 90+a FROM (SELECT a FROM t1 ORDER BY a LIMIT 3)
ORDER BY 1
LIMIT ii;
