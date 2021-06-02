-- wal4.test
-- 
-- execsql {
--     PRAGMA journal_mode=WAL;
--     CREATE TABLE t1(x);
--     INSERT INTO t1 VALUES(1);
--     INSERT INTO t1 VALUES(2);
--     SELECT x FROM t1 ORDER BY x;
-- }
PRAGMA journal_mode=WAL;
CREATE TABLE t1(x);
INSERT INTO t1 VALUES(1);
INSERT INTO t1 VALUES(2);
SELECT x FROM t1 ORDER BY x;