-- tkt3457.test
-- 
-- execsql {
--     CREATE TABLE t1(a, b, c);
--     INSERT INTO t1 VALUES(1, 2, 3);
--     BEGIN;
--     INSERT INTO t1 VALUES(4, 5, 6);
-- }
CREATE TABLE t1(a, b, c);
INSERT INTO t1 VALUES(1, 2, 3);
BEGIN;
INSERT INTO t1 VALUES(4, 5, 6);