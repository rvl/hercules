SET search_path TO hercules,public;

CREATE TABLE jobsets (
    branch_id integer PRIMARY KEY NOT NULL REFERENCES github_branches(id) ON DELETE CASCADE,
    errormsg text,
    errortime timestamptz,
    lastcheckedtime timestamptz,
    triggertime timestamptz,
    fetcherrormsg text,
    starttime timestamptz
);

CREATE TABLE jobs (
    jobset_id integer NOT NULL REFERENCES github_branches(id) ON DELETE CASCADE,
    name text NOT NULL,
    UNIQUE (jobset_id, name)
);
