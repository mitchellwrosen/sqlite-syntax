-- fts3fault.test
-- 
-- execsql { SELECT mit(matchinfo(t8, 's')) FROM t8 WHERE t8 MATCH 'a b c' }
SELECT mit(matchinfo(t8, 's')) FROM t8 WHERE t8 MATCH 'a b c'