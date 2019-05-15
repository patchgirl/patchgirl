-- !Ups

CREATE TABLE tree
(
  id SERIAL PRIMARY KEY,
  root JSON NOT NULL
);

-- !Downs

DROP TABLE tree CASCADE;
