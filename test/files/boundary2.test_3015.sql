-- boundary2.test
-- 
-- db eval {
--     SELECT a FROM t1 WHERE r < -9.22337303685477580800e+18 ORDER BY r DESC
-- }
SELECT a FROM t1 WHERE r < -9.22337303685477580800e+18 ORDER BY r DESC