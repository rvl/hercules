SET search_path TO public;

-- Derive projects from github repos.
-- One project per repo.
CREATE VIEW projects AS
    SELECT
        full_name AS name,
        name AS displayname,
        NULL::text AS description,
        enabled::integer AS enabled,
        0 as hidden,
        full_name as owner,
        remote_uri as homepage,
        NULL::text as declfile,
        NULL::text as decltype,
        NULL::text as declvalue
    FROM hercules.github_repos;

CREATE OR REPLACE FUNCTION posixtime(timestamptz) RETURNS integer
    AS 'SELECT cast(extract(epoch from $1) as integer)'
    LANGUAGE SQL
    IMMUTABLE
    RETURNS NULL ON NULL INPUT;

-- CREATE TABLE jobsets (
--     name text NOT NULL,
--     project text NOT NULL,
--     description text,
--     nixexprinput text NOT NULL,
--     nixexprpath text NOT NULL,
--     errormsg text,
--     errortime integer,
--     lastcheckedtime integer,
--     triggertime integer,
--     enabled integer DEFAULT 1 NOT NULL,
--     enableemail integer DEFAULT 1 NOT NULL,
--     hidden integer DEFAULT 0 NOT NULL,
--     emailoverride text NOT NULL,
--     keepnr integer DEFAULT 3 NOT NULL,
--     checkinterval integer DEFAULT 300 NOT NULL,
--     schedulingshares integer DEFAULT 100 NOT NULL,
--     fetcherrormsg text,
--     forceeval boolean,
--     starttime integer,
--     CONSTRAINT jobsets_schedulingshares_check CHECK ((schedulingshares > 0))
-- );

-- Derive jobsets from branches in github repos.
-- Branches may have evaluation status info attached.
CREATE VIEW jobsets AS
    SELECT
      hercules.github_branches.id AS id,
      hercules.github_branches.name AS name,
      hercules.github_repos.name AS project,
      NULL::text AS description,
      hercules.sources.path AS nixexprinput,
      hercules.github_branches.spec->>'jobset' AS nixexprpath,
      hercules.jobsets.errormsg AS errormsg,
      posixtime(hercules.jobsets.errortime) AS errortime,
      posixtime(hercules.jobsets.lastcheckedtime) AS lastcheckedtime,
      posixtime(hercules.jobsets.triggertime) AS triggertime,
      hercules.github_repos.enabled::integer AS enabled,
      0 AS enableemail,
      0 AS hidden,
      ''::text AS emailoverride,
      0 AS keepnr,
      0 AS checkinterval,
      100 AS schedulingshares,
      hercules.jobsets.fetcherrormsg AS fetcherrormsg,
      false AS forceeval,
      posixtime(hercules.jobsets.starttime) AS starttime
    FROM hercules.github_branches
      INNER JOIN hercules.github_repos ON (hercules.github_branches.repo_id = hercules.github_repos.id)
      INNER JOIN hercules.sources ON (hercules.sources.branch_id = hercules.github_branches.id)
      LEFT OUTER JOIN hercules.jobsets ON (hercules.jobsets.branch_id = hercules.github_branches.id)
    WHERE spec is NOT NULL;

-- When hydra-queue-runner updates the job status,
-- put the information in hercules jobsets table.
CREATE OR REPLACE FUNCTION jobsets_view_dml()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $function$
   BEGIN
      RAISE NOTICE 'jobsets_view_dml()';
      IF TG_OP = 'UPDATE' THEN
          UPDATE hercules.jobsets SET
              errormsg=NEW.errormsg,
              errortime=to_timestamp(NEW.errortime),
              lastcheckedtime=to_timestamp(NEW.lastcheckedtime),
              triggertime=to_timestamp(NEW.triggertime),
              fetcherrormsg=NEW.fetcherrormsg,
              starttime=to_timestamp(NEW.starttime)
              WHERE branch_id=OLD.id;
       -- UPDATE person_job SET pid=NEW.pid, job=NEW.job WHERE pid=OLD.pid;
       -- RETURN NEW;
      END IF;
      RETURN NEW;
    END;
$function$;

CREATE TRIGGER jobsets_view_dml_trig
    INSTEAD OF UPDATE ON jobsets
    FOR EACH ROW EXECUTE PROCEDURE jobsets_view_dml();

CREATE VIEW jobs AS
    SELECT
      hercules.github_repos.name AS project,
      hercules.github_branches.name AS jobset,
      hercules.jobs.name AS name
    FROM hercules.github_repos, hercules.github_branches, hercules.jobs
    WHERE hercules.github_branches.repo_id = hercules.github_repos.id
      AND hercules.jobs.jobset_id = hercules.github_branches.id;


--
-- Name: buildinputs; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

-- CREATE TABLE buildinputs (
--     id serial NOT NULL PRIMARY KEY,
--     build integer,
--     name text NOT NULL,
--     type text NOT NULL,
--     uri text,
--     revision text,
--     value text,
--     emailresponsible integer DEFAULT 0 NOT NULL,
--     dependency integer,
--     path text,
--     sha256hash text
-- );


--
-- Name: jobsetevalinputs; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE jobsetevalinputs (
    eval integer NOT NULL,
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



--
-- Name: jobsetevalmembers; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE jobsetevalmembers (
    eval integer NOT NULL,
    build integer NOT NULL,
    isnew integer NOT NULL
);



--
-- Name: jobsetevals; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE jobsetevals (
    id serial NOT NULL PRIMARY KEY,
    project text NOT NULL,
    jobset text NOT NULL,
    "timestamp" integer NOT NULL,
    checkouttime integer NOT NULL,
    evaltime integer NOT NULL,
    hasnewbuilds integer NOT NULL,
    hash text NOT NULL,
    nrbuilds integer,
    nrsucceeded integer
);

--
-- Name: jobsetinputalts; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE jobsetinputalts (
    project text NOT NULL,
    jobset text NOT NULL,
    input text NOT NULL,
    altnr integer NOT NULL,
    value text,
    revision text
);



--
-- Name: jobsetinputs; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE jobsetinputs (
    project text NOT NULL,
    jobset text NOT NULL,
    name text NOT NULL,
    type text NOT NULL,
    emailresponsible integer DEFAULT 0 NOT NULL
);
