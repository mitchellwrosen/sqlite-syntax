-- where7.test
-- 
-- execsql {
--     CREATE TABLE t1(a INTEGER PRIMARY KEY,b,c,d);
--     INSERT INTO t1 VALUES(1,2,3,4);
--     INSERT INTO t1 VALUES(2,3,4,5);
--     INSERT INTO t1 VALUES(3,4,6,8);
--     INSERT INTO t1 VALUES(4,5,10,15);
--     INSERT INTO t1 VALUES(5,10,100,1000);
--     CREATE INDEX t1b ON t1(b);
--     CREATE INDEX t1c ON t1(c);
--     SELECT * FROM t1;
-- }
CREATE TABLE t1(a INTEGER PRIMARY KEY,b,c,d);
INSERT INTO t1 VALUES(1,2,3,4);
INSERT INTO t1 VALUES(2,3,4,5);
INSERT INTO t1 VALUES(3,4,6,8);
INSERT INTO t1 VALUES(4,5,10,15);
INSERT INTO t1 VALUES(5,10,100,1000);
CREATE INDEX t1b ON t1(b);
CREATE INDEX t1c ON t1(c);
SELECT * FROM t1;