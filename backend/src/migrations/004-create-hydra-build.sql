--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = public, pg_catalog;

--
-- Name: modifynrbuildsfinished(); Type: FUNCTION; Schema: public; Owner: hydra
--

CREATE FUNCTION modifynrbuildsfinished() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  begin
    if ((tg_op = 'INSERT' and new.finished = 1) or
        (tg_op = 'UPDATE' and old.finished = 0 and new.finished = 1)) then
      update NrBuilds set count = count + 1 where what = 'finished';
    elsif ((tg_op = 'DELETE' and old.finished = 1) or
           (tg_op = 'UPDATE' and old.finished = 1 and new.finished = 0)) then
      update NrBuilds set count = count - 1 where what = 'finished';
    end if;
    return null;
  end;
$$;


--
-- Name: notifybuildbumped(); Type: FUNCTION; Schema: public; Owner: hydra
--

CREATE FUNCTION notifybuildbumped() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin notify builds_bumped; return null; end;$$;


--
-- Name: notifybuildcancelled(); Type: FUNCTION; Schema: public; Owner: hydra
--

CREATE FUNCTION notifybuildcancelled() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin notify builds_cancelled; return null; end;$$;


--
-- Name: notifybuildrestarted(); Type: FUNCTION; Schema: public; Owner: hydra
--

CREATE FUNCTION notifybuildrestarted() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin notify builds_restarted; return null; end;$$;


--
-- Name: notifybuildsdeleted(); Type: FUNCTION; Schema: public; Owner: hydra
--

CREATE FUNCTION notifybuildsdeleted() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin notify builds_deleted; return null; end;$$;

--
-- Name: notifyjobsetshareschanged(); Type: FUNCTION; Schema: public; Owner: hydra
--

CREATE FUNCTION notifyjobsetshareschanged() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin notify jobset_shares_changed; return null; end;$$;


SET default_tablespace = '';

SET default_with_oids = false;



--
-- Name: buildmetrics; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE buildmetrics (
    build integer NOT NULL,
    name text NOT NULL,
    unit text,
    value double precision NOT NULL,
    project text NOT NULL,
    jobset text NOT NULL,
    job text NOT NULL,
    "timestamp" integer NOT NULL
);



--
-- Name: buildoutputs; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE buildoutputs (
    build integer NOT NULL,
    name text NOT NULL,
    path text NOT NULL
);



--
-- Name: buildproducts; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE buildproducts (
    build integer NOT NULL,
    productnr integer NOT NULL,
    type text NOT NULL,
    subtype text NOT NULL,
    filesize bigint,
    sha1hash text,
    sha256hash text,
    path text,
    name text NOT NULL,
    defaultpath text
);



--
-- Name: builds; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE builds (
    id integer NOT NULL,
    finished integer NOT NULL,
    "timestamp" integer NOT NULL,
    project text NOT NULL,
    jobset text NOT NULL,
    job text NOT NULL,
    nixname text,
    description text,
    drvpath text NOT NULL,
    system text NOT NULL,
    license text,
    homepage text,
    maintainers text,
    maxsilent integer DEFAULT 3600,
    timeout integer DEFAULT 36000,
    ischannel integer DEFAULT 0 NOT NULL,
    iscurrent integer DEFAULT 0,
    nixexprinput text,
    nixexprpath text,
    priority integer DEFAULT 0 NOT NULL,
    globalpriority integer DEFAULT 0 NOT NULL,
    starttime integer,
    stoptime integer,
    iscachedbuild integer,
    buildstatus integer,
    size bigint,
    closuresize bigint,
    releasename text,
    keep integer DEFAULT 0 NOT NULL,
    notificationpendingsince integer,
    CONSTRAINT builds_check CHECK (((finished = 0) OR ((stoptime IS NOT NULL) AND (stoptime <> 0)))),
    CONSTRAINT builds_check1 CHECK (((finished = 0) OR ((starttime IS NOT NULL) AND (starttime <> 0))))
);



--
-- Name: builds_id_seq; Type: SEQUENCE; Schema: public; Owner: hydra
--

CREATE SEQUENCE builds_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: builds_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: hydra
--

ALTER SEQUENCE builds_id_seq OWNED BY builds.id;


--
-- Name: buildstepoutputs; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE buildstepoutputs (
    build integer NOT NULL,
    stepnr integer NOT NULL,
    name text NOT NULL,
    path text NOT NULL
);



--
-- Name: buildsteps; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE buildsteps (
    build integer NOT NULL,
    stepnr integer NOT NULL,
    type integer NOT NULL,
    drvpath text,
    busy integer NOT NULL,
    status integer,
    errormsg text,
    starttime integer,
    stoptime integer,
    machine text DEFAULT ''::text NOT NULL,
    system text,
    propagatedfrom integer,
    overhead integer,
    timesbuilt integer,
    isnondeterministic boolean
);


--
-- Name: failedpaths; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE failedpaths (
    path text NOT NULL
);





--
-- Name: newsitems; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE newsitems (
    id integer NOT NULL,
    contents text NOT NULL,
    createtime integer NOT NULL,
    author text NOT NULL
);


--
-- Name: nrbuilds; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE nrbuilds (
    what text NOT NULL,
    count integer NOT NULL
);





--
-- Name: releasemembers; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE releasemembers (
    project text NOT NULL,
    release_ text NOT NULL,
    build integer NOT NULL,
    description text
);



--
-- Name: releases; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE releases (
    project text NOT NULL,
    name text NOT NULL,
    "timestamp" integer NOT NULL,
    description text
);



--
-- Name: systemstatus; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE systemstatus (
    what text NOT NULL,
    status json NOT NULL
);



--
-- Name: systemtypes; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE systemtypes (
    system text NOT NULL,
    maxconcurrent integer DEFAULT 2 NOT NULL
);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: hydra
--

ALTER TABLE ONLY builds ALTER COLUMN id SET DEFAULT nextval('builds_id_seq'::regclass);

