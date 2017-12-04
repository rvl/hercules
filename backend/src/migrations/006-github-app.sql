SET search_path TO hercules,public;

CREATE TABLE github_app (
    app_id integer NOT NULL,
    updated_at timestamptz NOT NULL
);
