--- Databases

SELECT * FROM Databases;

--- Users

select count(*) from Users;

--- Tests

Select * From Categories;

SELECT Titre, NoCassette FROM Rubriques
WHERE Titre LIKE 'Woody%'
OR NoCassette=5;

--- Inserts

INSERT INTO Cassettes(NoCassette) VALUES(105);
INSERT INTO Cassettes(NoCassette) VALUES(106);
INSERT INTO Cassettes(NoCassette) VALUES(107);
