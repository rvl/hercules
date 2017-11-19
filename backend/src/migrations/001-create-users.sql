CREATE SCHEMA hercules;

CREATE TABLE hercules.users(
  id bigserial PRIMARY KEY NOT NULL,
  name text,
  email text UNIQUE,
  github_id text UNIQUE,
  github_token bytea UNIQUE
);
