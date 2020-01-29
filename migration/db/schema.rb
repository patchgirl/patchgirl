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
-- Name: request_node_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.request_node_type AS ENUM (
    'RequestFolder',
    'RequestFile'
);


--
-- Name: request_nodes_as_json(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.request_nodes_as_json(node_id integer) RETURNS jsonb[]
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
        INNER JOIN request_collection_to_request_node2 rcrn ON rcrn.request_node_id = rn.id
        WHERE rcrn.request_collection_id = rc_id;
        RETURN result;
      END;
      $$;


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: account; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.account (
    id integer NOT NULL,
    email public.citext NOT NULL,
    signup_token text DEFAULT md5((random())::text) NOT NULL,
    password text
);


--
-- Name: account_environment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.account_environment (
    account_id integer NOT NULL,
    environment_id integer NOT NULL
);


--
-- Name: account_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.account_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: account_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.account_id_seq OWNED BY public.account.id;


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
    id integer NOT NULL,
    name text NOT NULL
);


--
-- Name: environment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.environment_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: environment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.environment_id_seq OWNED BY public.environment.id;


--
-- Name: key_value; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.key_value (
    id integer NOT NULL,
    environment_id integer,
    key text NOT NULL,
    value text NOT NULL
);


--
-- Name: key_value_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.key_value_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: key_value_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.key_value_id_seq OWNED BY public.key_value.id;


--
-- Name: request_collection; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.request_collection (
    id integer NOT NULL,
    account_id integer
);


--
-- Name: request_collection2; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.request_collection2 (
    id integer NOT NULL,
    account_id integer
);


--
-- Name: request_collection2_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.request_collection2_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: request_collection2_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.request_collection2_id_seq OWNED BY public.request_collection2.id;


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
    request_node_id integer NOT NULL
);


--
-- Name: request_collection_to_request_node2; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.request_collection_to_request_node2 (
    request_collection_id integer NOT NULL,
    request_node_id integer NOT NULL
);


--
-- Name: request_node; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.request_node (
    id integer NOT NULL,
    request_node_parent_id integer,
    tag public.request_node_type NOT NULL,
    name text NOT NULL,
    http_url text,
    http_method public.http_method_type,
    http_headers public.header_type[],
    http_body text,
    CONSTRAINT request_node_check CHECK ((((tag = 'RequestFolder'::public.request_node_type) AND (http_url IS NULL) AND (http_method IS NULL) AND (http_headers IS NULL) AND (http_body IS NULL)) OR ((tag = 'RequestFile'::public.request_node_type) AND (http_url IS NOT NULL) AND (http_method IS NOT NULL) AND (http_headers IS NOT NULL) AND (http_body IS NOT NULL))))
);


--
-- Name: request_node_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.request_node_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: request_node_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.request_node_id_seq OWNED BY public.request_node.id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying NOT NULL
);


--
-- Name: account id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.account ALTER COLUMN id SET DEFAULT nextval('public.account_id_seq'::regclass);


--
-- Name: environment id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.environment ALTER COLUMN id SET DEFAULT nextval('public.environment_id_seq'::regclass);


--
-- Name: key_value id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.key_value ALTER COLUMN id SET DEFAULT nextval('public.key_value_id_seq'::regclass);


--
-- Name: request_collection id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection ALTER COLUMN id SET DEFAULT nextval('public.request_collection_id_seq'::regclass);


--
-- Name: request_collection2 id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection2 ALTER COLUMN id SET DEFAULT nextval('public.request_collection2_id_seq'::regclass);


--
-- Name: request_node id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_node ALTER COLUMN id SET DEFAULT nextval('public.request_node_id_seq'::regclass);


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
-- Name: request_collection2 request_collection2_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection2
    ADD CONSTRAINT request_collection2_pkey PRIMARY KEY (id);


--
-- Name: request_collection request_collection_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection
    ADD CONSTRAINT request_collection_pkey PRIMARY KEY (id);


--
-- Name: request_collection_to_request_node2 request_collection_to_request_node2_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection_to_request_node2
    ADD CONSTRAINT request_collection_to_request_node2_pkey PRIMARY KEY (request_collection_id, request_node_id);


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
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


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
-- Name: request_collection2 request_collection2_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection2
    ADD CONSTRAINT request_collection2_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;


--
-- Name: request_collection request_collection_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection
    ADD CONSTRAINT request_collection_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;


--
-- Name: request_collection_to_request_node2 request_collection_to_request_node2_request_collection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection_to_request_node2
    ADD CONSTRAINT request_collection_to_request_node2_request_collection_id_fkey FOREIGN KEY (request_collection_id) REFERENCES public.request_collection2(id) ON DELETE CASCADE;


--
-- Name: request_collection_to_request_node2 request_collection_to_request_node2_request_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection_to_request_node2
    ADD CONSTRAINT request_collection_to_request_node2_request_node_id_fkey FOREIGN KEY (request_node_id) REFERENCES public.request_node(id) ON DELETE CASCADE;


--
-- Name: request_collection_to_request_node request_collection_to_request_node_request_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection_to_request_node
    ADD CONSTRAINT request_collection_to_request_node_request_node_id_fkey FOREIGN KEY (request_node_id) REFERENCES public.request_node(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

SET search_path TO "$user", public;

INSERT INTO "schema_migrations" (version) VALUES
('20191021075512');


