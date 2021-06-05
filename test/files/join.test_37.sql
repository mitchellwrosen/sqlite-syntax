-- join.test
-- 
-- execsql {
--       SELECT * FROM t12 NATURAL LEFT JOIN t13
--       EXCEPT
--       SELECT * FROM t12 NATURAL LEFT JOIN (SELECT * FROM t13 WHERE b>0);
-- }
SELECT * FROM t12 NATURAL LEFT JOIN t13
EXCEPT
SELECT * FROM t12 NATURAL LEFT JOIN (SELECT * FROM t13 WHERE b>0);