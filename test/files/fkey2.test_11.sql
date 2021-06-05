-- fkey2.test
-- 
-- execsql {
--     CREATE TABLE ab(a PRIMARY KEY, b);
--     CREATE TABLE cd(
--       c PRIMARY KEY REFERENCES ab ON UPDATE CASCADE ON DELETE CASCADE, 
--       d
--     );
--     CREATE TABLE ef(
--       e REFERENCES cd ON UPDATE CASCADE, 
--       f, CHECK (e!=5)
--     );
-- }
CREATE TABLE ab(a PRIMARY KEY, b);
CREATE TABLE cd(
c PRIMARY KEY REFERENCES ab ON UPDATE CASCADE ON DELETE CASCADE, 
d
);
CREATE TABLE ef(
e REFERENCES cd ON UPDATE CASCADE, 
f, CHECK (e!=5)
);