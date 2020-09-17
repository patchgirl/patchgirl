SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: citext; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS citext WITH SCHEMA public;


--
-- Name: EXTENSION citext; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION citext IS 'data type for case-insensitive character strings';


--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: header_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.header_type AS (
	header_key text,
	header_value text
);


--
-- Name: http_method_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.http_method_type AS ENUM (
    'Get',
    'Post',
    'Put',
    'Delete',
    'Patch',
    'Head',
    'Options'
);


--
-- Name: pg_node_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.pg_node_type AS ENUM (
    'PgFolder',
    'PgFile'
);


--
-- Name: request_node_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.request_node_type AS ENUM (
    'RequestFolder',
    'RequestFile'
);


--
-- Name: scenario_node_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.scenario_node_type AS ENUM (
    'ScenarioFolder',
    'ScenarioFile'
);


--
-- Name: scene_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.scene_type AS ENUM (
    'HttpActor',
    'PgActor'
);


--
-- Name: pg_nodes_as_json(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.pg_nodes_as_json(node_id uuid) RETURNS jsonb[]
    LANGUAGE plpgsql
    AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'PgFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(pg_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'sql', sql,
        'pg_host', pg_host,
        'pg_password', pg_password,
        'pg_port', pg_port,
        'pg_user', pg_user,
        'pg_dbname', pg_dbname
      )
    END
  ) INTO result
  FROM pg_node
  WHERE pg_node_parent_id = node_id;
  RETURN result;
END;
$$;


--
-- Name: request_nodes_as_json(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.request_nodes_as_json(node_id uuid) RETURNS jsonb[]
    LANGUAGE plpgsql
    AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'RequestFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(request_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'http_body', http_body,
        'http_url', http_url,
        'http_method', http_method,
        'http_headers', http_headers
      )
    END
  ) INTO result
  FROM request_node
  WHERE request_node_parent_id = node_id;
  RETURN result;
END;
$$;


--
-- Name: root_pg_nodes_as_json(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.root_pg_nodes_as_json(rc_id uuid) RETURNS jsonb[]
    LANGUAGE plpgsql
    AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'PgFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(pg_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'sql', sql,
        'pg_host', pg_host,
        'pg_password', pg_password,
        'pg_port', pg_port,
        'pg_user', pg_user,
        'pg_dbname', pg_dbname
      )
    END
  ) INTO result
  FROM pg_node rn
  INNER JOIN pg_collection_to_pg_node rcrn ON rcrn.pg_actor_id = rn.id
  WHERE rcrn.pg_collection_id = rc_id;
  RETURN result;
END;
$$;


--
-- Name: root_request_nodes_as_json(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.root_request_nodes_as_json(rc_id integer) RETURNS jsonb[]
    LANGUAGE plpgsql
    AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'RequestFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(request_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'http_body', http_body,
        'http_url', http_url,
        'http_method', http_method,
        'http_headers', http_headers
      )
    END
  ) INTO result
  FROM request_node rn
  INNER JOIN request_collection_to_request_node rcrn ON rcrn.request_node_id = rn.id
  WHERE rcrn.request_collection_id = rc_id;
  RETURN result;
END;
$$;


--
-- Name: root_scenario_nodes_as_json(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.root_scenario_nodes_as_json(rc_id uuid) RETURNS jsonb[]
    LANGUAGE plpgsql
    AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'ScenarioFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(scenario_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'scene_nodes', root_scene_node_as_json(scene_node_id),
        'environment_id', environment_id
      )
    END
  ) INTO result
  FROM scenario_node rn
  INNER JOIN scenario_collection_to_scenario_node rcrn ON rcrn.scenario_node_id = rn.id
  WHERE rcrn.scenario_collection_id = rc_id;
  RETURN result;
END;
$$;


--
-- Name: root_scene_node_as_json(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.root_scene_node_as_json(node_id uuid) RETURNS jsonb[]
    LANGUAGE plpgsql
    AS $$
DECLARE result jsonb[];
BEGIN
  SELECT
    array_agg(
      jsonb_build_object(
        'id', id,
        'scene_node_parent_id', scene_node_parent_id,
        'actor_type', actor_type,
        'http_actor_id', http_actor_id,
        'pg_actor_id', pg_actor_id,
        'prescript', prescript,
        'postscript', postscript
      )
    ) || scene_node_as_json(node_id)
  INTO result
  FROM scene_node
  WHERE id = node_id;
  RETURN result || ARRAY[]::jsonb[]; -- always returns empty array if result is NULL;
END;
$$;


--
-- Name: scenario_nodes_as_json(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.scenario_nodes_as_json(node_id uuid) RETURNS jsonb[]
    LANGUAGE plpgsql
    AS $$
DECLARE result jsonb[];
BEGIN
  SELECT array_agg (
    CASE WHEN tag = 'ScenarioFolder' THEN
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'children', COALESCE(scenario_nodes_as_json(id), '{}'::jsonb[])
      )
    ELSE
      jsonb_build_object(
        'id', id,
        'name', name,
        'tag', tag,
        'scene_nodes', root_scene_node_as_json(scene_node_id),
        'environment_id', environment_id
      )
    END
  ) INTO result
  FROM scenario_node
  WHERE scenario_node_parent_id = node_id;
  RETURN result;
END;
$$;


--
-- Name: scene_node_as_json(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.scene_node_as_json(node_id uuid) RETURNS jsonb[]
    LANGUAGE plpgsql
    AS $$
DECLARE result jsonb[];
BEGIN
  SELECT
    array_agg(
      jsonb_build_object(
        'id', id,
        'scene_node_parent_id', scene_node_parent_id,
        'actor_type', actor_type,
        'http_actor_id', http_actor_id,
        'pg_actor_id', pg_actor_id,
        'prescript', prescript,
        'postscript', postscript
      )
    ) || scene_node_as_json(id)
  INTO result
  FROM scene_node
  WHERE scene_node_parent_id = node_id
  GROUP BY id;
  RETURN result;
END;
$$;


SET default_tablespace = '';

--
-- Name: account; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.account (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    github_id integer NOT NULL,
    email public.citext
);


--
-- Name: account_environment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.account_environment (
    account_id uuid NOT NULL,
    environment_id uuid NOT NULL
);


--
-- Name: ar_internal_metadata; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ar_internal_metadata (
    key character varying NOT NULL,
    value character varying,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: environment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.environment (
    id uuid NOT NULL,
    name text NOT NULL
);


--
-- Name: key_value; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.key_value (
    id uuid NOT NULL,
    environment_id uuid,
    key text NOT NULL,
    value text NOT NULL,
    hidden boolean NOT NULL
);


--
-- Name: pg_collection; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pg_collection (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    account_id uuid
);


--
-- Name: pg_collection_to_pg_node; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pg_collection_to_pg_node (
    pg_collection_id uuid NOT NULL,
    pg_actor_id uuid NOT NULL
);


--
-- Name: pg_node; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pg_node (
    id uuid NOT NULL,
    pg_node_parent_id uuid,
    tag public.pg_node_type NOT NULL,
    name text NOT NULL,
    sql text,
    pg_host text,
    pg_password text,
    pg_port text,
    pg_user text,
    pg_dbname text,
    CONSTRAINT pg_node_check CHECK ((((tag = 'PgFolder'::public.pg_node_type) AND (sql IS NULL)) OR ((tag = 'PgFile'::public.pg_node_type) AND (sql IS NOT NULL) AND (pg_host IS NOT NULL) AND (pg_password IS NOT NULL) AND (pg_port IS NOT NULL) AND (pg_user IS NOT NULL) AND (pg_dbname IS NOT NULL))))
);


--
-- Name: request_collection; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.request_collection (
    id integer NOT NULL,
    account_id uuid
);


--
-- Name: request_collection_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.request_collection_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: request_collection_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.request_collection_id_seq OWNED BY public.request_collection.id;


--
-- Name: request_collection_to_request_node; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.request_collection_to_request_node (
    request_collection_id integer NOT NULL,
    request_node_id uuid NOT NULL
);


--
-- Name: request_node; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.request_node (
    id uuid NOT NULL,
    request_node_parent_id uuid,
    tag public.request_node_type NOT NULL,
    name text NOT NULL,
    http_url text,
    http_method public.http_method_type,
    http_headers public.header_type[],
    http_body text,
    CONSTRAINT request_node_check CHECK ((((tag = 'RequestFolder'::public.request_node_type) AND (http_url IS NULL) AND (http_method IS NULL) AND (http_headers IS NULL) AND (http_body IS NULL)) OR ((tag = 'RequestFile'::public.request_node_type) AND (http_url IS NOT NULL) AND (http_method IS NOT NULL) AND (http_headers IS NOT NULL) AND (http_body IS NOT NULL))))
);


--
-- Name: scenario_collection; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.scenario_collection (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    account_id uuid
);


--
-- Name: scenario_collection_to_scenario_node; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.scenario_collection_to_scenario_node (
    scenario_collection_id uuid NOT NULL,
    scenario_node_id uuid NOT NULL
);


--
-- Name: scenario_node; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.scenario_node (
    id uuid NOT NULL,
    tag public.scenario_node_type NOT NULL,
    environment_id uuid,
    name text NOT NULL,
    scenario_node_parent_id uuid,
    scene_node_id uuid,
    CONSTRAINT scenario_node_check CHECK (((tag = 'ScenarioFile'::public.scenario_node_type) OR ((tag = 'ScenarioFolder'::public.scenario_node_type) AND (environment_id IS NULL) AND (scene_node_id IS NULL))))
);


--
-- Name: scene_node; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.scene_node (
    id uuid NOT NULL,
    scene_node_parent_id uuid,
    actor_type public.scene_type NOT NULL,
    http_actor_id uuid,
    pg_actor_id uuid,
    prescript text NOT NULL,
    postscript text NOT NULL,
    CONSTRAINT scene_node_check CHECK ((((actor_type = 'HttpActor'::public.scene_type) AND (http_actor_id IS NOT NULL) AND (pg_actor_id IS NULL)) OR ((actor_type = 'PgActor'::public.scene_type) AND (pg_actor_id IS NOT NULL) AND (http_actor_id IS NULL))))
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying NOT NULL
);


--
-- Name: user_test; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_test (
    id integer NOT NULL,
    role text NOT NULL,
    firstname text NOT NULL,
    lastname text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    deleted_at timestamp with time zone
);


--
-- Name: user_test_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.user_test_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_test_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.user_test_id_seq OWNED BY public.user_test.id;


--
-- Name: request_collection id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection ALTER COLUMN id SET DEFAULT nextval('public.request_collection_id_seq'::regclass);


--
-- Name: user_test id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_test ALTER COLUMN id SET DEFAULT nextval('public.user_test_id_seq'::regclass);


--
-- Name: account account_email_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_email_key UNIQUE (email);


--
-- Name: account_environment account_environment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account_environment
    ADD CONSTRAINT account_environment_pkey PRIMARY KEY (account_id, environment_id);


--
-- Name: account account_github_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_github_id_key UNIQUE (github_id);


--
-- Name: account account_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);


--
-- Name: ar_internal_metadata ar_internal_metadata_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ar_internal_metadata
    ADD CONSTRAINT ar_internal_metadata_pkey PRIMARY KEY (key);


--
-- Name: environment environment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.environment
    ADD CONSTRAINT environment_pkey PRIMARY KEY (id);


--
-- Name: key_value key_value_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.key_value
    ADD CONSTRAINT key_value_pkey PRIMARY KEY (id);


--
-- Name: pg_collection pg_collection_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pg_collection
    ADD CONSTRAINT pg_collection_pkey PRIMARY KEY (id);


--
-- Name: pg_collection_to_pg_node pg_collection_to_pg_node_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pg_collection_to_pg_node
    ADD CONSTRAINT pg_collection_to_pg_node_pkey PRIMARY KEY (pg_collection_id, pg_actor_id);


--
-- Name: pg_node pg_node_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pg_node
    ADD CONSTRAINT pg_node_pkey PRIMARY KEY (id);


--
-- Name: request_collection request_collection_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection
    ADD CONSTRAINT request_collection_pkey PRIMARY KEY (id);


--
-- Name: request_collection_to_request_node request_collection_to_request_node_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection_to_request_node
    ADD CONSTRAINT request_collection_to_request_node_pkey PRIMARY KEY (request_collection_id, request_node_id);


--
-- Name: request_node request_node_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_node
    ADD CONSTRAINT request_node_pkey PRIMARY KEY (id);


--
-- Name: scenario_collection scenario_collection_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scenario_collection
    ADD CONSTRAINT scenario_collection_pkey PRIMARY KEY (id);


--
-- Name: scenario_collection_to_scenario_node scenario_collection_to_scenario_node_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scenario_collection_to_scenario_node
    ADD CONSTRAINT scenario_collection_to_scenario_node_pkey PRIMARY KEY (scenario_collection_id, scenario_node_id);


--
-- Name: scenario_node scenario_node_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scenario_node
    ADD CONSTRAINT scenario_node_pkey PRIMARY KEY (id);


--
-- Name: scene_node scene_node_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scene_node
    ADD CONSTRAINT scene_node_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: user_test user_test_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_test
    ADD CONSTRAINT user_test_pkey PRIMARY KEY (id);


--
-- Name: account_environment account_environment_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account_environment
    ADD CONSTRAINT account_environment_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;


--
-- Name: account_environment account_environment_environment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account_environment
    ADD CONSTRAINT account_environment_environment_id_fkey FOREIGN KEY (environment_id) REFERENCES public.environment(id) ON DELETE CASCADE;


--
-- Name: key_value key_value_environment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.key_value
    ADD CONSTRAINT key_value_environment_id_fkey FOREIGN KEY (environment_id) REFERENCES public.environment(id) ON DELETE CASCADE;


--
-- Name: pg_collection pg_collection_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pg_collection
    ADD CONSTRAINT pg_collection_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;


--
-- Name: pg_collection_to_pg_node pg_collection_to_pg_node_pg_actor_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pg_collection_to_pg_node
    ADD CONSTRAINT pg_collection_to_pg_node_pg_actor_id_fkey FOREIGN KEY (pg_actor_id) REFERENCES public.pg_node(id) ON DELETE CASCADE;


--
-- Name: pg_collection_to_pg_node pg_collection_to_pg_node_pg_collection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pg_collection_to_pg_node
    ADD CONSTRAINT pg_collection_to_pg_node_pg_collection_id_fkey FOREIGN KEY (pg_collection_id) REFERENCES public.pg_collection(id) ON DELETE CASCADE;


--
-- Name: pg_node pg_node_pg_node_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pg_node
    ADD CONSTRAINT pg_node_pg_node_parent_id_fkey FOREIGN KEY (pg_node_parent_id) REFERENCES public.pg_node(id) ON DELETE CASCADE;


--
-- Name: request_collection request_collection_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection
    ADD CONSTRAINT request_collection_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;


--
-- Name: request_collection_to_request_node request_collection_to_request_node_request_collection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection_to_request_node
    ADD CONSTRAINT request_collection_to_request_node_request_collection_id_fkey FOREIGN KEY (request_collection_id) REFERENCES public.request_collection(id) ON DELETE CASCADE;


--
-- Name: request_collection_to_request_node request_collection_to_request_node_request_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection_to_request_node
    ADD CONSTRAINT request_collection_to_request_node_request_node_id_fkey FOREIGN KEY (request_node_id) REFERENCES public.request_node(id) ON DELETE CASCADE;


--
-- Name: request_node request_node_request_node_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_node
    ADD CONSTRAINT request_node_request_node_parent_id_fkey FOREIGN KEY (request_node_parent_id) REFERENCES public.request_node(id) ON DELETE CASCADE;


--
-- Name: scenario_collection scenario_collection_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scenario_collection
    ADD CONSTRAINT scenario_collection_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;


--
-- Name: scenario_collection_to_scenario_node scenario_collection_to_scenario_nod_scenario_collection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scenario_collection_to_scenario_node
    ADD CONSTRAINT scenario_collection_to_scenario_nod_scenario_collection_id_fkey FOREIGN KEY (scenario_collection_id) REFERENCES public.scenario_collection(id) ON DELETE CASCADE;


--
-- Name: scenario_collection_to_scenario_node scenario_collection_to_scenario_node_scenario_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scenario_collection_to_scenario_node
    ADD CONSTRAINT scenario_collection_to_scenario_node_scenario_node_id_fkey FOREIGN KEY (scenario_node_id) REFERENCES public.scenario_node(id) ON DELETE CASCADE;


--
-- Name: scenario_node scenario_node_environment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scenario_node
    ADD CONSTRAINT scenario_node_environment_id_fkey FOREIGN KEY (environment_id) REFERENCES public.environment(id);


--
-- Name: scenario_node scenario_node_scenario_node_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scenario_node
    ADD CONSTRAINT scenario_node_scenario_node_parent_id_fkey FOREIGN KEY (scenario_node_parent_id) REFERENCES public.scenario_node(id) ON DELETE CASCADE;


--
-- Name: scenario_node scenario_node_scene_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scenario_node
    ADD CONSTRAINT scenario_node_scene_node_id_fkey FOREIGN KEY (scene_node_id) REFERENCES public.scene_node(id) ON DELETE CASCADE;


--
-- Name: scene_node scene_node_http_actor_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scene_node
    ADD CONSTRAINT scene_node_http_actor_id_fkey FOREIGN KEY (http_actor_id) REFERENCES public.request_node(id);


--
-- Name: scene_node scene_node_pg_actor_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scene_node
    ADD CONSTRAINT scene_node_pg_actor_id_fkey FOREIGN KEY (pg_actor_id) REFERENCES public.pg_node(id);


--
-- Name: scene_node scene_node_scene_node_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.scene_node
    ADD CONSTRAINT scene_node_scene_node_parent_id_fkey FOREIGN KEY (scene_node_parent_id) REFERENCES public.scene_node(id);


--
-- PostgreSQL database dump complete
--

SET search_path TO "$user", public;

INSERT INTO "schema_migrations" (version) VALUES
('20191021075512');


