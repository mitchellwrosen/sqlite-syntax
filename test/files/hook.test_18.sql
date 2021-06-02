-- hook.test
-- 
-- execsql {
--       SELECT * FROM t1 UNION SELECT * FROM t3;
--       SELECT * FROM t1 UNION ALL SELECT * FROM t3;
--       SELECT * FROM t1 INTERSECT SELECT * FROM t3;
--       SELECT * FROM t1 EXCEPT SELECT * FROM t3;
--       SELECT * FROM t1 ORDER BY b;
--       SELECT * FROM t1 GROUP BY b;
-- }
SELECT * FROM t1 UNION SELECT * FROM t3;
SELECT * FROM t1 UNION ALL SELECT * FROM t3;
SELECT * FROM t1 INTERSECT SELECT * FROM t3;
SELECT * FROM t1 EXCEPT SELECT * FROM t3;
SELECT * FROM t1 ORDER BY b;
SELECT * FROM t1 GROUP BY b;