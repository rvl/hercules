-- TODO: these will be views on hercules tables

--
-- Name: buildinputs; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE buildinputs (
    id integer NOT NULL,
    build integer,
    name text NOT NULL,
    type text NOT NULL,
    uri text,
    revision text,
    value text,
    emailresponsible integer DEFAULT 0 NOT NULL,
    dependency integer,
    path text,
    sha256hash text
);

--
-- Name: buildinputs_id_seq; Type: SEQUENCE; Schema: public; Owner: hydra
--

CREATE SEQUENCE buildinputs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



--
-- Name: buildinputs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: hydra
--

ALTER SEQUENCE buildinputs_id_seq OWNED BY buildinputs.id;

--
-- Name: jobs; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE jobs (
    project text NOT NULL,
    jobset text NOT NULL,
    name text NOT NULL
);



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
    id integer NOT NULL,
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
-- Name: jobsetevals_id_seq; Type: SEQUENCE; Schema: public; Owner: hydra
--

CREATE SEQUENCE jobsetevals_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



--
-- Name: jobsetevals_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: hydra
--

ALTER SEQUENCE jobsetevals_id_seq OWNED BY jobsetevals.id;


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



--
-- Name: jobsetrenames; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE jobsetrenames (
    project text NOT NULL,
    from_ text NOT NULL,
    to_ text NOT NULL
);



--
-- Name: jobsets; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE jobsets (
    name text NOT NULL,
    project text NOT NULL,
    description text,
    nixexprinput text NOT NULL,
    nixexprpath text NOT NULL,
    errormsg text,
    errortime integer,
    lastcheckedtime integer,
    triggertime integer,
    enabled integer DEFAULT 1 NOT NULL,
    enableemail integer DEFAULT 1 NOT NULL,
    hidden integer DEFAULT 0 NOT NULL,
    emailoverride text NOT NULL,
    keepnr integer DEFAULT 3 NOT NULL,
    checkinterval integer DEFAULT 300 NOT NULL,
    schedulingshares integer DEFAULT 100 NOT NULL,
    fetcherrormsg text,
    forceeval boolean,
    starttime integer,
    CONSTRAINT jobsets_schedulingshares_check CHECK ((schedulingshares > 0))
);


--
-- Name: projectmembers; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE projectmembers (
    project text NOT NULL,
    username text NOT NULL
);



--
-- Name: projects; Type: TABLE; Schema: public; Owner: hydra; Tablespace: 
--

CREATE TABLE projects (
    name text NOT NULL,
    displayname text NOT NULL,
    description text,
    enabled integer DEFAULT 1 NOT NULL,
    hidden integer DEFAULT 0 NOT NULL,
    owner text NOT NULL,
    homepage text,
    declfile text,
    decltype text,
    declvalue text
);



--
-- Name: id; Type: DEFAULT; Schema: public; Owner: hydra
--

ALTER TABLE ONLY buildinputs ALTER COLUMN id SET DEFAULT nextval('buildinputs_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: hydra
--

ALTER TABLE ONLY jobsetevals ALTER COLUMN id SET DEFAULT nextval('jobsetevals_id_seq'::regclass);
