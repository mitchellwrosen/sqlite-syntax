-- subquery.test
-- 
-- execsql {
--     CREATE TABLE t7(c7);
--     INSERT INTO t7 VALUES(1);
--     INSERT INTO t7 VALUES(2);
--     INSERT INTO t7 VALUES(3);
--     CREATE TABLE t8(c8);
--     INSERT INTO t8 VALUES(100);
--     INSERT INTO t8 VALUES(200);
--     INSERT INTO t8 VALUES(300);
--     CREATE TABLE t9(c9);
--     INSERT INTO t9 VALUES(10000);
--     INSERT INTO t9 VALUES(20000);
--     INSERT INTO t9 VALUES(30000);
-- 
--     SELECT (SELECT c7+c8 FROM t7) FROM t8;
-- }
CREATE TABLE t7(c7);
INSERT INTO t7 VALUES(1);
INSERT INTO t7 VALUES(2);
INSERT INTO t7 VALUES(3);
CREATE TABLE t8(c8);
INSERT INTO t8 VALUES(100);
INSERT INTO t8 VALUES(200);
INSERT INTO t8 VALUES(300);
CREATE TABLE t9(c9);
INSERT INTO t9 VALUES(10000);
INSERT INTO t9 VALUES(20000);
INSERT INTO t9 VALUES(30000);
SELECT (SELECT c7+c8 FROM t7) FROM t8;