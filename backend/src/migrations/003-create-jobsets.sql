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

--
-- Name: jobsetevals; Type: TABLE; Schema: public; Owner: hydra; Tablespace:
--

CREATE TABLE jobsetevals (
    id serial PRIMARY KEY NOT NULL,
    project text NOT NULL,
    jobset_id integer NOT NULL REFERENCES github_branches(id) ON DELETE CASCADE,
    "timestamp" integer NOT NULL,
    checkouttime integer NOT NULL,
    evaltime integer NOT NULL,
    hasnewbuilds integer NOT NULL,
    hash text NOT NULL,
    nrbuilds integer,
    nrsucceeded integer
);


CREATE TABLE jobsetevalmembers (
    eval integer NOT NULL REFERENCES jobsetevals(id),
    build integer NOT NULL,
    isnew integer NOT NULL
);

CREATE TABLE jobsetevalinputs (
    eval integer NOT NULL REFERENCES jobsetevals(id),
    name text NOT NULL,
    altnr integer NOT NULL,
    type text NOT NULL,
    uri text,
    revision text,
    value text,
    dependency integer,
    path text,
    sha256hash text
);
