-- collate5.test
-- 
-- execsql {
--     SELECT a FROM collate5t2 INTERSECT select a FROM collate5t1 WHERE a != 'a';
-- }
SELECT a FROM collate5t2 INTERSECT select a FROM collate5t1 WHERE a != 'a';