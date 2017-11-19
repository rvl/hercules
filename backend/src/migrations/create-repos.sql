CREATE TABLE github_repos (
  id integer PRIMARY KEY NOT NULL,
  name text NOT NULL,
  full_name text NOT NULL,
  default_branch text NOT NULL,
  remote_uri text NOT NULL,
  enabled boolean NOT NULL
  -- possibly adding fork information would make cloning quicker
);

CREATE TABLE github_branches (
  id bigserial PRIMARY KEY NOT NULL,
  repo_id integer NOT NULL REFERENCES github_repos(id) ON DELETE CASCADE,
  name text NOT NULL,
  head text NOT NULL, -- TODO: pg sha1 type?
  spec jsonb NOT NULL, -- contents of .hercules.yml
  -- pull_request_number integer REFERENCES github_pull_requests(number) ON DELETE CASCADE,
  UNIQUE (repo_id, name)
);

CREATE TABLE github_pull_requests (
  number integer PRIMARY KEY NOT NULL,
  repo_id integer NOT NULL REFERENCES github_repos(id) ON DELETE CASCADE,
  title text NOT NULL
);

-- A full bare clone of the github repo
CREATE TABLE github_repo_cache (
  repo_id integer NOT NULL REFERENCES github_repos(id),
  path text NOT NULL,  -- github.com_owner/repo.git
  last_fetch TIMESTAMP
);
