CREATE TABLE parents AS
  SELECT "ace" AS parent, "bella" AS child UNION
  SELECT "ace"          , "charlie"        UNION
  SELECT "daisy"        , "hank"           UNION
  SELECT "finn"         , "ace"            UNION
  SELECT "finn"         , "daisy"          UNION
  SELECT "finn"         , "ginger"         UNION
  SELECT "ellie"        , "finn";

CREATE TABLE dogs AS
  SELECT "ace" AS name, "long" AS fur, 26 AS height UNION
  SELECT "bella"      , "short"      , 52           UNION
  SELECT "charlie"    , "long"       , 47           UNION
  SELECT "daisy"      , "long"       , 46           UNION
  SELECT "ellie"      , "short"      , 35           UNION
  SELECT "finn"       , "curly"      , 32           UNION
  SELECT "ginger"     , "short"      , 28           UNION
  SELECT "hank"       , "curly"      , 31;

CREATE TABLE sizes AS
  SELECT "toy" AS size, 24 AS min, 28 AS max UNION
  SELECT "mini"       , 28       , 35        UNION
  SELECT "medium"     , 35       , 45        UNION
  SELECT "standard"   , 45       , 60;


-- All dogs with parents ordered by decreasing height of their parent
CREATE TABLE by_parent_height AS
  SELECT child FROM parents, dogs WHERE parent = name ORDER BY height DESC;


-- The size of each dog
CREATE TABLE size_of_dogs AS
  SELECT name, size FROM dogs, sizes WHERE height > min AND height <= max;


-- [Optional] Filling out this helper table is recommended
CREATE TABLE siblings AS
  SELECT
  s1.child AS sibling1, s2.child AS sibling2, size
  FROM
  parents AS s1
  JOIN
  parents AS s2 ON s1.parent = s2.parent AND s1.child < s2.child
  JOIN
  dogs AS d1 ON s1.child = d1.name
  JOIN
  dogs AS d2 ON s2.child = d2.name
  JOIN
  sizes ON d1.height BETWEEN min AND max
             AND d2.height BETWEEN min AND max
  WHERE
  d1.height BETWEEN min AND max
  AND d2.height BETWEEN min AND max;

-- Sentences about siblings that are the same size
CREATE TABLE sentences AS
  SELECT "The two siblings, " || sibling1 || " and " || sibling2 || ", have the same size: " || size AS sentence
  FROM siblings;


-- Height range for each fur type where all of the heights differ by no more than 30% from the average height
CREATE TABLE low_variance AS
  SELECT
  fur, MAX(height) - MIN(height)
  FROM
  dogs
  GROUP BY
  fur
  HAVING
    MIN(height) >= 0.7 * AVG(height) AND
    MAX(height) <= 1.3 * AVG(height);

