-- e_fkey.test
-- 
-- execsql {
--     UPDATE zeus SET a = '1';
--     SELECT typeof(c), c, typeof(d), d FROM apollo;
-- }
UPDATE zeus SET a = '1';
SELECT typeof(c), c, typeof(d), d FROM apollo;