-- rowid.test
-- 
-- execsql {
--       INSERT INTO t2(b) VALUES(22);
--       INSERT INTO t2(b) VALUES(33);
--       INSERT INTO t2(b) VALUES(44);
--       INSERT INTO t2(b) VALUES(55);
--       SELECT b FROM t2 WHERE a NOT IN(1,2,1000000,1000001,2147483647) 
--           ORDER BY b;
-- }
INSERT INTO t2(b) VALUES(22);
INSERT INTO t2(b) VALUES(33);
INSERT INTO t2(b) VALUES(44);
INSERT INTO t2(b) VALUES(55);
SELECT b FROM t2 WHERE a NOT IN(1,2,1000000,1000001,2147483647) 
ORDER BY b;