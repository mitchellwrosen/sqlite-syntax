-- select8.test
-- 
-- execsql {
--     SELECT DISTINCT artist,sum(timesplayed) AS total      
--     FROM songs      
--     GROUP BY LOWER(artist)      
--     LIMIT 1 OFFSET 1
-- }
SELECT DISTINCT artist,sum(timesplayed) AS total      
FROM songs      
GROUP BY LOWER(artist)      
LIMIT 1 OFFSET 1