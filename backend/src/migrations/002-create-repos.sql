SET search_path TO hercules, public, pg_catalog;

CREATE TABLE github_app (
    app_id integer NOT NULL PRIMARY KEY,
    updated_at timestamptz NOT NULL
);

CREATE TABLE github_installation (
    id integer PRIMARY KEY NOT NULL,
    app_id integer NOT NULL REFERENCES github_app(app_id),
    account_id integer NOT NULL
);

CREATE TABLE github_repos (
  id integer PRIMARY KEY NOT NULL,
  name text NOT NULL,
  full_name text NOT NULL,
  default_branch text NOT NULL,
  remote_uri text NOT NULL,
  enabled boolean NOT NULL DEFAULT FALSE,
  installation_id integer REFERENCES github_installation(id)
  -- possibly adding fork information would make cloning quicker
);

CREATE TABLE github_pull_requests (
  number integer NOT NULL,
  repo_id integer NOT NULL REFERENCES github_repos(id) ON DELETE CASCADE,
  title text NOT NULL,
  -- fixme: add pull request head
  PRIMARY KEY (number, repo_id)
);

CREATE TABLE github_branches (
  id serial PRIMARY KEY NOT NULL,
  repo_id integer NOT NULL REFERENCES github_repos(id) ON DELETE CASCADE,
  name text NOT NULL, -- ref name
  rev text NOT NULL, -- TODO: maybe bytea(20)
  spec jsonb NOT NULL, -- contents of .hercules.yml
  pull_request_number integer,
  FOREIGN KEY (pull_request_number, repo_id) REFERENCES github_pull_requests(number, repo_id) ON DELETE CASCADE,
  UNIQUE (repo_id, name)
);

-- A full bare clone of the github repo
CREATE TABLE github_repo_cache (
  repo_id integer NOT NULL REFERENCES github_repos(id),
  path text NOT NULL,  -- github/owner/repo.git
  start_fetch timestamptz,
  last_fetch timestamptz
);

-- A working tree of the repo, copied to the nix store.
CREATE TABLE sources (
  branch_id integer PRIMARY KEY NOT NULL REFERENCES github_branches(id) ON DELETE CASCADE,
  path text NOT NULL,
  sha256 text NOT NULL
);
