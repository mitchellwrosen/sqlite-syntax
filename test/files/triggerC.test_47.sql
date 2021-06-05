-- triggerC.test
-- 
-- execsql {
--     CREATE TRIGGER t9r1 AFTER DELETE ON t9 BEGIN
--       DELETE FROM t9 WHERE b=old.a;
--     END;
--     DELETE FROM t9 WHERE b=4;
--     SELECT a FROM t9 ORDER BY a;
-- }
CREATE TRIGGER t9r1 AFTER DELETE ON t9 BEGIN
DELETE FROM t9 WHERE b=old.a;
END;
DELETE FROM t9 WHERE b=4;
SELECT a FROM t9 ORDER BY a;