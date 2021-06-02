-- select6.test
-- 
-- execsql {
--       SELECT * FROM (
--         SELECT x AS 'a' FROM t1 INTERSECT SELECT x+1 AS 'a' FROM t1
--       ) ORDER BY a;
-- }
SELECT * FROM (
SELECT x AS 'a' FROM t1 INTERSECT SELECT x+1 AS 'a' FROM t1
) ORDER BY a;