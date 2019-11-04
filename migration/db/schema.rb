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


SET default_tablespace = '';

SET default_with_oids = false;

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
-- Name: request_node_as_js(public.request_node); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.request_node_as_js(somerow public.request_node) RETURNS jsonb
    LANGUAGE plpgsql
    AS $$
      BEGIN
        RETURN CASE WHEN someRow.tag = 'RequestFolder' THEN
          jsonb_build_object(
            'id', someRow.id,
            'name', someRow.name,
            'tag', someRow.tag,
            'children', json_build_array()
          )
        ELSE
          jsonb_build_object(
            'id', someRow.id,
            'name', someRow.name,
            'tag', someRow.tag,
            'http_url', someRow.http_url,
            'http_method', someRow.http_method,
            'http_headers', someRow.http_headers,
            'http_body', someRow.http_body
          )
        END;
      END;
      $$;


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
-- Name: request_collection_to_request_node; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.request_collection_to_request_node (
    request_collection_id integer NOT NULL,
    request_node_id integer NOT NULL
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
-- Name: request_node id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_node ALTER COLUMN id SET DEFAULT nextval('public.request_node_id_seq'::regclass);


--
-- Name: ar_internal_metadata ar_internal_metadata_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ar_internal_metadata
    ADD CONSTRAINT ar_internal_metadata_pkey PRIMARY KEY (key);


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
-- Name: request_collection_to_request_node request_collection_to_request_node_request_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.request_collection_to_request_node
    ADD CONSTRAINT request_collection_to_request_node_request_node_id_fkey FOREIGN KEY (request_node_id) REFERENCES public.request_node(id);


--
-- PostgreSQL database dump complete
--

SET search_path TO "$user", public;

INSERT INTO "schema_migrations" (version) VALUES
('20191021075512');


